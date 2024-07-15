const std = @import("std");

pub const LaunchOption = struct {
    display_name: []const u8,
    search_string: []const u8,
    launch_action: LaunchAction,

    pub fn deinit(self: LaunchOption, allocator: std.mem.Allocator) void {
        allocator.free(self.display_name);
        allocator.free(self.search_string);
        switch (self.launch_action) {
            LaunchAction.exec => |exec| allocator.free(exec),
            LaunchAction.typeout => |typeout| allocator.free(typeout),
        }
    }
};

pub const LaunchAction = union(enum) {
    exec: []const u8,
    typeout: []const u8,
};
