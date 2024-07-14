const std = @import("std");
const LaunchOption = @import("./launch_option.zig").LaunchOption;

pub fn options(allocator: std.mem.Allocator) ![]const LaunchOption {
    const xdg_data_dirs_string = try std.process.getEnvVarOwned(allocator, "XDG_DATA_DIRS");
    var xdg_data_dirs = std.mem.splitScalar(u8, xdg_data_dirs_string, ':');
    var result = std.ArrayList(LaunchOption).init(allocator);
    while (xdg_data_dirs.next()) |dir_path| {
        var dir = std.fs.openDirAbsolute(dir_path, .{ .iterate = true }) catch continue;
        defer dir.close();
        var walker = try dir.walk(allocator);
        defer walker.deinit();
        while (try walker.next()) |desktop_file_path| {
            const option = LaunchOption{
                .display_name = desktop_file_path.path,
                .search_string = "search",
                .launch_action = .{ .exec = "exec" },
            };
            try result.append(option);
        }
    }
    return result.toOwnedSlice();
}
