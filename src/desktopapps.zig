const std = @import("std");
const LaunchOption = @import("./launch_option.zig").LaunchOption;

const allocator = std.heap.page_allocator;

pub fn options() ![]const LaunchOption {
    const xdg_data_dirs_string = try std.process.getEnvVarOwned(allocator, "XDG_DATA_DIRS");
    var xdg_data_dirs = std.mem.splitScalar(u8, xdg_data_dirs_string, ':');
    while (xdg_data_dirs.next()) |dir_path| {
        const dir = std.fs.openIterableDirAbsolute(dir_path, .{}) catch continue;
        var walker = try dir.walk(allocator);
        while (try walker.next()) |desktop_file_path| {
            std.debug.print("{s}\n", .{desktop_file_path.path});
        }
        defer walker.deinit();
    }
    return &[_]LaunchOption{};
}
