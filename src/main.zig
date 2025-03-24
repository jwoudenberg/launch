const std = @import("std");
const desktopapps = @import("./desktopapps.zig");
const LaunchOption = @import("./launch_option.zig").LaunchOption;

const ETX: u8 = 3; // Ctrl+C
const EOT: u8 = 4; // Ctrl+D
const ESC: u8 = 27; // Escape
const DEL: u8 = 127; // Backspace
const NAK: u8 = 21; // Ctrl+U
const CR: u8 = 13; // Enter
const SI: u8 = 14; // Ctrl+N
const DLE: u8 = 16; // Ctrl+P

const allocator = std.heap.page_allocator;

pub fn main() !void {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = gpa_state.allocator();
    const stdin_file = std.io.getStdIn();
    const stdin = stdin_file.reader();

    const stderr_file = std.io.getStdErr();
    const stderr = stderr_file.writer();

    const old_mode = try std.posix.tcgetattr(stdin_file.handle);
    defer std.posix.tcsetattr(stdin_file.handle, std.posix.TCSA.DRAIN, old_mode) catch @panic("terminal borked");
    try set_raw(stdin_file.handle, old_mode);
    try run(gpa, stdin, stderr);
}

const State = struct {
    desktopapp_options: []const LaunchOption,
    typed: std.ArrayListUnmanaged(u8),
};

fn run(gpa: std.mem.Allocator, reader: anytype, writer: anytype) !void {
    const desktopapp_options = try desktopapps.options(gpa);
    defer gpa.free(desktopapp_options);
    var state = State{
        .desktopapp_options = desktopapp_options,
        .typed = std.ArrayListUnmanaged(u8){},
    };
    while (true) {
        try render(state, writer);
        const byte: u8 = try reader.readByte();
        switch (byte) {
            ETX,
            EOT,
            ESC,
            => {
                break;
            },
            CR => {
                break;
            },
            DEL => {
                _ = state.typed.pop();
            },
            NAK => {
                state.typed.clearAndFree(gpa);
            },
            else => {
                try state.typed.append(gpa, byte);
            },
        }
    }
}

fn render(state: State, writer: anytype) !void {
    try writer.writeAll(&.{ ESC, '[', '2', 'J' });
    for (state.desktopapp_options) |option| {
        try writer.writeAll(&.{ '\n', ESC, '[', '0', 'G' });
        try writer.writeAll(option.display_name);
    }
    try writer.writeAll(&.{ '\n', ESC, '[', '0', 'G' });
    try writer.writeAll(state.typed.items);
}

// Set terminal to raw mode, to get more control over how the terminal is
// rendered. Based off the way Nim does this in its terminal library:
// https://github.com/nim-lang/Nim/blob/version-2-0/lib/pure/terminal.nim#L258
// Some documentation for what all these options do can be found in `man stty`.
fn set_raw(handle: std.fs.File.Handle, base_mode: std.posix.termios) !void {
    var mode = base_mode;
    mode.iflag.BRKINT = false;
    mode.iflag.ICRNL = false;
    mode.iflag.INPCK = false;
    mode.iflag.ISTRIP = false;
    mode.iflag.IXON = false;
    mode.oflag.OPOST = false;
    mode.cflag.CSIZE = std.posix.CSIZE.CS8;
    mode.cflag.PARENB = false;
    mode.lflag.ECHO = false;
    mode.lflag.ICANON = false;
    mode.lflag.IEXTEN = false;
    mode.lflag.ISIG = false;
    mode.cc[@intFromEnum(std.posix.V.MIN)] = 1;
    mode.cc[@intFromEnum(std.posix.V.TIME)] = 0;
    try std.posix.tcsetattr(handle, std.posix.TCSA.FLUSH, mode);
}
