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

const MAX_CHAR_WIDTH = 80;

pub fn main() !void {
    var arena_state = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // Don't deinit the arena - we'll end the program immediately after we're done with it.
    const arena = arena_state.allocator();
    const stdin_file = std.io.getStdIn();
    const stdin = stdin_file.reader();

    const stderr_file = std.io.getStdErr();
    const stderr = stderr_file.writer();

    const old_mode = try std.posix.tcgetattr(stdin_file.handle);
    defer std.posix.tcsetattr(stdin_file.handle, std.posix.TCSA.DRAIN, old_mode) catch @panic("terminal borked");
    try set_raw(stdin_file.handle, old_mode);
    try run(arena, stdin, stderr);
}

const State = struct {
    allocator: std.mem.Allocator,
    options: []const LaunchOption,
    offsets: std.SegmentedList(OptionOffsets, 0),
    typed: std.BoundedArray(u8, MAX_CHAR_WIDTH),

    fn init(allocator: std.mem.Allocator, options: []const LaunchOption) State {
        return State{
            .allocator = allocator,
            .options = options,
            .offsets = std.SegmentedList(OptionOffsets, 0){},
            .typed = std.BoundedArray(u8, MAX_CHAR_WIDTH).init(0) catch unreachable,
        };
    }

    fn deinit(self: *State) void {
        defer self.offsets.deinit(self.allocator);
    }
};

const OptionOffsets = struct {
    option_index: u32,
    match_count: u16,
    match_index: u16,
};

fn run(allocator: std.mem.Allocator, reader: anytype, writer: anytype) !void {
    const options = try desktopapps.options(allocator);
    defer allocator.free(options);
    var state = State.init(allocator, options);
    defer state.deinit();
    while (true) {
        try render(&state, writer);
        const keypress: u8 = try reader.readByte();
        if (try handle_keypress(keypress, &state)) break;
    }
}

fn handle_keypress(keypress: u8, state: *State) !bool {
    switch (keypress) {
        ETX,
        EOT,
        ESC,
        => {
            return true;
        },
        CR => {
            return true;
        },
        DEL => {
            _ = state.typed.pop();
            var offset_iter = state.offsets.constIterator(0);
            var new_offset_len: usize = 0;
            while (offset_iter.next()) |offset| {
                if (offset.match_count > state.typed.len) {
                    state.offsets.len = new_offset_len;
                    break;
                } else {
                    new_offset_len += 1;
                }
            }
        },
        NAK => {
            state.typed.clear();
            state.offsets.clearRetainingCapacity();
        },
        else => {
            if (state.typed.len >= MAX_CHAR_WIDTH) return false;
            var offset_iter = state.offsets.constIterator(0);
            if (state.typed.len > 0) {
                while (offset_iter.next()) |offset| {
                    if (offset.match_count > state.typed.len) break;
                    if (offset.match_count < state.typed.len) continue;
                    const option = state.options[offset.option_index];
                    if (std.mem.indexOfScalarPos(u8, option.search_string.slice(), offset.match_index + 1, keypress)) |index| {
                        try state.offsets.append(state.allocator, .{
                            .match_index = @truncate(index),
                            .option_index = offset.option_index,
                            .match_count = offset.match_count + 1,
                        });
                    }
                }
            } else {
                for (state.options, 0..) |option, option_index| {
                    if (std.mem.indexOfScalar(u8, option.search_string.slice(), keypress)) |index| {
                        try state.offsets.append(state.allocator, .{
                            .match_index = @truncate(index),
                            .option_index = @truncate(option_index),
                            .match_count = 1,
                        });
                    }
                }
            }
            state.typed.append(keypress) catch unreachable;
        },
    }
    return false;
}

test "keypresses update state.typed" {
    var state = State.init(std.testing.allocator, &.{});
    defer state.deinit();

    try std.testing.expectEqualStrings("", state.typed.slice());

    _ = try handle_keypress('h', &state);
    try std.testing.expectEqualStrings("h", state.typed.slice());

    _ = try handle_keypress('i', &state);
    try std.testing.expectEqualStrings("hi", state.typed.slice());

    _ = try handle_keypress(DEL, &state);
    try std.testing.expectEqualStrings("h", state.typed.slice());

    _ = try handle_keypress('e', &state);
    _ = try handle_keypress('l', &state);
    _ = try handle_keypress('l', &state);
    _ = try handle_keypress('o', &state);
    try std.testing.expectEqualStrings("hello", state.typed.slice());

    _ = try handle_keypress(NAK, &state);
    try std.testing.expectEqualStrings("", state.typed.slice());
}

test "keypresses narrow option selection" {
    const options = .{
        testOption("zero"),
        testOption("one"),
        testOption("two"),
    };
    var state = State.init(std.testing.allocator, &options);
    defer state.deinit();

    _ = try handle_keypress('o', &state);
    try std.testing.expectEqualSlices(u32, &.{ 0, 1, 2 }, testMatchingOptions(&state).slice());

    _ = try handle_keypress('e', &state);
    try std.testing.expectEqualSlices(u32, &.{1}, testMatchingOptions(&state).slice());

    _ = try handle_keypress(DEL, &state);
    try std.testing.expectEqualSlices(u32, &.{ 0, 1, 2 }, testMatchingOptions(&state).slice());
}

fn testOption(name: []const u8) LaunchOption {
    return LaunchOption.init(
        name,
        name,
        .{ .exec = "" },
    );
}

fn testMatchingOptions(state: *State) std.BoundedArray(u32, 64) {
    var result = std.BoundedArray(u32, 64).init(0) catch unreachable;
    var offsetIter = state.offsets.constIterator(0);
    while (offsetIter.next()) |offset| {
        if (offset.match_count < state.typed.len) continue;
        if (offset.match_count > state.typed.len) break;
        result.append(offset.option_index) catch @panic("overflow test matching options");
    }
    return result;
}

fn render(state: *const State, writer: anytype) !void {
    try writer.writeAll(&.{ ESC, '[', '2', 'J' });
    if (state.typed.len > 0) {
        var offset_iter = state.offsets.constIterator(0);
        while (offset_iter.next()) |offset| {
            if (offset.match_count < state.typed.len) continue;
            const option = state.options[offset.option_index];
            try writer.writeAll(&.{ '\n', ESC, '[', '0', 'G' });
            try writer.writeAll(option.display_name.slice());
        }
    } else {
        for (state.options) |option| {
            try writer.writeAll(&.{ '\n', ESC, '[', '0', 'G' });
            try writer.writeAll(option.display_name.slice());
        }
    }
    try writer.writeAll(&.{ '\n', ESC, '[', '0', 'G' });
    try writer.writeAll(state.typed.slice());
}

test render {
    const options = .{
        testOption("zero"),
        testOption("one"),
        testOption("two"),
    };
    var state = State.init(std.testing.allocator, &options);
    defer state.deinit();

    var output = std.BoundedArray(u8, 1024).init(0) catch unreachable;
    try render(&state, output.writer());
    try std.testing.expectEqualSlices(u8, &.{
        ESC, '[', '2',  'J',  '\n', ESC, '[', '0', 'G',
        'z', 'e', 'r',  'o',  '\n', ESC, '[', '0', 'G',
        'o', 'n', 'e',  '\n', ESC,  '[', '0', 'G', 't',
        'w', 'o', '\n', ESC,  '[',  '0', 'G',
    }, output.slice());
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
