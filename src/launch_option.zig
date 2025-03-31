const std = @import("std");

pub const LaunchOption = struct {
    display_name: std.BoundedArray(u8, 80),
    search_string: std.BoundedArray(u8, 80),
    launch_action: LaunchAction,

    pub fn init(
        display_name: []const u8,
        search_string: []const u8,
        launch_action: LaunchAction,
    ) LaunchOption {
        return .{
            .display_name = boundedArrayToSlice(display_name),
            .search_string = boundedArrayToSlice(search_string),
            .launch_action = launch_action,
        };
    }

    pub fn deinit(self: LaunchOption, allocator: std.mem.Allocator) void {
        allocator.free(self.display_name);
        allocator.free(self.search_string);
        switch (self.launch_action) {
            LaunchAction.exec => |exec| allocator.free(exec),
            LaunchAction.typeout => |typeout| allocator.free(typeout),
        }
    }
};

fn boundedArrayToSlice(slice: []const u8) std.BoundedArray(u8, 80) {
    const truncated_slice = slice[0..@min(slice.len, 80)];
    return std.BoundedArray(u8, 80).fromSlice(truncated_slice) catch unreachable;
}

pub const LaunchAction = union(enum) {
    exec: []const u8,
    typeout: []const u8,
};
