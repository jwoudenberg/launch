const std = @import("std");
const desktopapps = @import("./desktopapps.zig");

pub fn main() !void {
    // const events = std.event.Channel(Event);
    // std.Thread.spawn(.{}, render, events);
    try listen();
}

const Event = union {
    input: u8,
};

const ETX: u8 = 3; // Ctrl+C
const EOT: u8 = 4; // Ctrl+D
const ESC: u8 = 27; // Escape
const DEL: u8 = 127; // Backspace
const NAK: u8 = 21; // Ctrl+U
const CR: u8 = 13; // Enter
const SI: u8 = 14; // Ctrl+N
const DLE: u8 = 16; // Ctrl+P

fn listen() !void {
    _ = try desktopapps.options();

    const stdin_file = std.io.getStdIn();
    const stdin = stdin_file.reader();

    const stderr_file = std.io.getStdErr();
    const stderr = stderr_file.writer();

    const old_mode = try std.os.tcgetattr(stdin_file.handle);
    defer std.os.tcsetattr(stdin_file.handle, std.os.TCSA.DRAIN, old_mode) catch @panic("terminal borked");
    try set_raw(stdin_file.handle, old_mode);

    while (true) {
        const byte: u8 = try stdin.readByte();
        switch (byte) {
            ETX,
            EOT,
            ESC,
            => {
                break;
            },
            CR => {
                std.debug.print("Done!", .{});
                break;
            },
            DEL => {
                try stderr.writeAll(&.{ ESC, '[', 'D', ESC, '[', 'K' });
            },
            NAK => {
                try stderr.writeAll(&.{ ESC, '[', 'G', ESC, '[', 'K' });
            },
            else => {
                try stderr.writeByte(byte);
            },
        }
    }
}

fn render(events: std.event.Channel(Event)) void {
    _ = desktopapps.options();
    while (true) {
        _ = events.get();
    }
}

// Set terminal to raw mode, to get more control over how the terminal is
// rendered. Based off the way Nim does this in its terminal library:
// https://github.com/nim-lang/Nim/blob/version-2-0/lib/pure/terminal.nim#L258
// Some documentation for what all these options do can be found in `man stty`.
fn set_raw(handle: std.fs.File.Handle, base_mode: std.os.termios) !void {
    var mode = base_mode;
    mode.iflag &= ~std.os.linux.BRKINT;
    mode.iflag &= ~std.os.linux.ICRNL;
    mode.iflag &= ~std.os.linux.INPCK;
    mode.iflag &= ~std.os.linux.ISTRIP;
    mode.iflag &= ~std.os.linux.IXON;
    mode.oflag &= ~std.os.linux.OPOST;
    mode.cflag &= ~std.os.linux.CSIZE;
    mode.cflag &= ~std.os.linux.PARENB;
    mode.cflag |= std.os.linux.CS8;
    mode.lflag &= ~std.os.linux.ECHO;
    mode.lflag &= ~std.os.linux.ICANON;
    mode.lflag &= ~std.os.linux.IEXTEN;
    mode.lflag &= ~std.os.linux.ISIG;
    mode.cc[std.os.linux.V.MIN] = 1;
    mode.cc[std.os.linux.V.TIME] = 0;
    try std.os.tcsetattr(handle, std.os.TCSA.FLUSH, mode);
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
