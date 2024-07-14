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
            const option = try parseDesktopAppFile(allocator, path);
            try launch_options.append(option);
        }
    }
    const result = try launch_options.toOwnedSlice();
    std.sort.block(LaunchOption, result, {}, cmpByNameLength);
    return result;
}

fn cmpByNameLength(_: void, a: LaunchOption, b: LaunchOption) bool {
    return a.display_name.len < b.display_name.len;
}

fn parseDesktopAppFile(allocator: std.mem.Allocator, path: []const u8) !LaunchOption {
    std.debug.print("{s}\n", .{path}); // TODO: remove this
    var file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    var display_name: []const u8 = undefined;
    var search_string: []const u8 = undefined;
    var exec: []const u8 = undefined;

    var reader = file.reader();
    var buf: [1024]u8 = undefined;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        display_name = try allocator.dupe(u8, "name");
        search_string = try std.ascii.allocLowerString(allocator, display_name);
        exec = line;
    }

    return LaunchOption{
        .display_name = display_name,
        .search_string = search_string,
        .launch_action = .{ .exec = exec },
    };
}
