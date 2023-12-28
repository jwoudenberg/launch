pub const LaunchOption = struct {
    display_name: []const u8,
    search_string: []const u8,
    matching_indexes: []const u8,
    launch_action: LaunchAction,
};

pub const LaunchAction = union {
    exec: []const u8,
    typeout: []const u8,
};
