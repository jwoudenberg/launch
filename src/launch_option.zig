const std = @import("std");

pub const LaunchOption = struct {
    display_name: std.BoundedArray(u8, 80),
    search_string: std.BoundedArray(u8, 80),
    launch_function: *const fn (self: *const LaunchOption) anyerror!void,

    pub fn launch(self: *const LaunchOption) !void {
        try self.launch_function(self);
    }

    pub fn toBoundedArray(slice: []const u8) std.BoundedArray(u8, 80) {
        const truncated_slice = slice[0..@min(slice.len, 80)];
        return std.BoundedArray(u8, 80).fromSlice(truncated_slice) catch unreachable;
    }
};
