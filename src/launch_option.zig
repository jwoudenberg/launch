const std = @import("std");

pub const LaunchOption = struct {
    display_name: std.BoundedArray(u8, 80),
    search_string: std.BoundedArray(u8, 80),
    launch_function: *const fn (self: *const LaunchOption, allocator: std.mem.Allocator) anyerror!void,

    pub fn launch(self: *const LaunchOption, allocator: std.mem.Allocator) !void {
        try self.launch_function(self, allocator);
    }

    pub fn toBoundedArray(slice: []const u8) std.BoundedArray(u8, 80) {
        const truncated_slice = slice[0..@min(slice.len, 80)];
        return std.BoundedArray(u8, 80).fromSlice(truncated_slice) catch unreachable;
    }

    pub fn cmpByNameLength(_: void, a: *const LaunchOption, b: *const LaunchOption) bool {
        return a.display_name.len > b.display_name.len;
    }
};
