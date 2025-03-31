const std = @import("std");
const LaunchOption = @import("./launch_option.zig").LaunchOption;

pub fn options(allocator: std.mem.Allocator) ![]const LaunchOption {
    const xdg_data_dirs_string = try std.process.getEnvVarOwned(allocator, "XDG_DATA_DIRS");
    var xdg_data_dirs = std.mem.splitScalar(u8, xdg_data_dirs_string, ':');
    var launch_options = std.ArrayList(LaunchOption).init(allocator);
    while (xdg_data_dirs.next()) |dir_path| {
        const apps_path = try std.fs.path.join(allocator, &[_][]const u8{ dir_path, "applications" });
        defer allocator.free(apps_path);

        var dir = std.fs.openDirAbsolute(apps_path, .{ .iterate = true }) catch continue;
        defer dir.close();

        var walker = try dir.walk(allocator);
        defer walker.deinit();

        while (try walker.next()) |desktop_file_path| {
            if (!std.mem.endsWith(u8, desktop_file_path.path, ".desktop")) {
                continue;
            }
            const path = try std.fs.path.join(allocator, &[_][]const u8{ apps_path, desktop_file_path.path });
            defer allocator.free(path);

            var file = try std.fs.openFileAbsolute(path, .{});
            defer file.close();

            const option = try parseDesktopAppFile(allocator, file.reader().any());
            try launch_options.append(option orelse continue);
        }
    }
    const result = try launch_options.toOwnedSlice();
    std.sort.block(LaunchOption, result, {}, cmpByNameLength);
    return result;
}

fn cmpByNameLength(_: void, a: LaunchOption, b: LaunchOption) bool {
    return a.display_name.len < b.display_name.len;
}

// Parse a .desktop file to find the name of the app it describes, along with the way to launch it.
fn parseDesktopAppFile(allocator: std.mem.Allocator, file: std.io.AnyReader) !?LaunchOption {
    var display_name: ?[]const u8 = null;
    var search_string: ?[]const u8 = null;
    var exec: ?[]const u8 = null;
    var in_desktop_entry: bool = false;

    var buf: [1024]u8 = undefined;
    while (try file.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (std.mem.eql(u8, line, "[Desktop Entry]")) {
            in_desktop_entry = true;
            continue;
        }
        if (line.len > 0 and line[0] == '[') {
            in_desktop_entry = false;
            continue;
        }
        if (!in_desktop_entry) {
            continue;
        }

        const index = std.mem.indexOf(u8, line, "=") orelse continue;

        const key = line[0..index];
        const val = line[index + 1 ..];
        if (std.mem.eql(u8, key, "Name")) {
            display_name = try allocator.dupe(u8, val);
            search_string = try std.ascii.allocLowerString(allocator, val);
        } else if (std.mem.eql(u8, key, "Exec")) {
            exec = try removeExecArgs(allocator, val);
        }
    }

    return LaunchOption.init(
        display_name orelse return null,
        search_string orelse return null,
        .{ .exec = exec orelse return null },
    );
}

test "parseDesktopAppFile: valid file" {
    const allocator = std.testing.allocator;
    const contents =
        \\[Fluff]
        \\Name=Red Herring
        \\
        \\[Desktop Entry]
        \\Name=My App
        \\Exec=/bin/run-me %F
        \\
        \\[Footer]
        \\Name=Ignore Me!
    ;
    var file = std.io.fixedBufferStream(contents);
    const result = try parseDesktopAppFile(allocator, file.reader().any());
    try std.testing.expectEqualStrings("My App", result.?.display_name);
    try std.testing.expectEqualStrings("my app", result.?.search_string);
    try std.testing.expectEqualStrings("/bin/run-me", result.?.launch_action.exec);
    result.?.deinit(allocator);
}

test "parseDesktopAppFile: empty file" {
    const allocator = std.testing.allocator;
    const contents = "";
    var file = std.io.fixedBufferStream(contents);
    const result = try parseDesktopAppFile(allocator, file.reader().any());
    try std.testing.expect(result == null);
}

// Take the value of an Exec=.. string and remove any placeholders like %F and %u.
fn removeExecArgs(allocator: std.mem.Allocator, exec: []u8) ![]const u8 {
    var copyIndex: u16 = 0;
    var pasteIndex: u16 = 0;

    // Rewrite the exec slice, replacing placeholders like %U and %f.
    while (copyIndex < exec.len) {
        if (exec[copyIndex] == '%') {
            copyIndex += 2;
        } else {
            exec[pasteIndex] = exec[copyIndex];
            copyIndex += 1;
            pasteIndex += 1;
        }
    }

    // Go back to remove trailing whitespace.
    while (pasteIndex > 0 and exec[pasteIndex - 1] == ' ') {
        pasteIndex -= 1;
    }

    return allocator.dupe(u8, exec[0..pasteIndex]);
}

test "removeExecArgs" {
    const allocator = std.testing.allocator;
    const contents = try allocator.dupe(u8, "hi %uthere %F  ");
    defer allocator.free(contents);
    const result = try removeExecArgs(allocator, contents);
    try std.testing.expectEqualStrings("hi there", result);
    allocator.free(result);
}
