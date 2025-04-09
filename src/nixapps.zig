const std = @import("std");
const desktopapps = @import("./desktopapps.zig");
const LaunchOption = @import("./launch_option.zig").LaunchOption;

pub fn addOptions(
    allocator: std.mem.Allocator,
    options: *std.ArrayListUnmanaged(*const LaunchOption),
) !void {
    var nix_locale_proc = std.process.Child.init(&.{
        "nix-locate",
        "--top-level",
        "--regex",
        "^/share/applications/.*\\.desktop$",
    }, allocator);
    nix_locale_proc.stdout_behavior = .Pipe;
    try nix_locale_proc.spawn();
    const stdout = nix_locale_proc.stdout orelse return error.NixLocateStdoutNotFound;
    var reader = stdout.reader();
    var buf: [4096]u8 = undefined;
    const start_index = options.items.len;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try parseNixLocateLine(allocator, options, line);
    }
    std.sort.block(*const LaunchOption, options.items[start_index..], {}, LaunchOption.cmpByNameLength);
}

fn parseNixLocateLine(
    allocator: std.mem.Allocator,
    options: *std.ArrayListUnmanaged(*const LaunchOption),
    line: []const u8,
) !void {
    var tokens = std.mem.tokenizeAny(u8, line, " \t");
    const option = try allocator.create(NixAppLaunchOption);
    option.launch_option.launch_function = &NixAppLaunchOption.launch;
    try options.append(allocator, &option.launch_option);

    const derivation_name = tokens.next() orelse return error.NixLocateWithoutNameColumn;
    // The derivation_name ends with '.out'. We strip that off below.
    const app_name = try allocator.dupe(u8, derivation_name[0 .. derivation_name.len - 4]);
    option.app_name = app_name;
    option.launch_option.display_name = LaunchOption.toBoundedArray(app_name);
    const search_string = try std.ascii.allocLowerString(allocator, app_name);
    option.launch_option.search_string = LaunchOption.toBoundedArray(search_string);

    _ = tokens.next() orelse return error.NixLocateWithoutColumn2;
    _ = tokens.next() orelse return error.NixLocateWithoutColumn3;
    var desktop_file_path = tokens.next() orelse return error.NixLocateWithoutDesktopFileColumn;
    // The desktop file path will be something like:
    //     /nix/store/123-my-app/some/path/to/app.desktop
    //                           ^^^^^^^^^^^^^^^^^^^^^^^^
    // We'd like to keep the path from the root nix directory,
    // so we can find the path in a different version of the
    // same derivation (assuming it hasn't moved in that version).
    for (0..3) |_| {
        const next_sep_index = std.mem.indexOfScalarPos(u8, desktop_file_path, 1, std.fs.path.sep) orelse return error.NixPathShorterThanExpected;
        desktop_file_path = desktop_file_path[next_sep_index..];
    }
    option.desktop_file = try allocator.dupe(u8, desktop_file_path[1..]);
}

const NixAppLaunchOption = struct {
    launch_option: LaunchOption,
    app_name: []const u8,
    desktop_file: []const u8,

    fn launch(option: *const LaunchOption, allocator: std.mem.Allocator) !void {
        const self: *const NixAppLaunchOption = @fieldParentPtr("launch_option", option);
        var nix_build_proc = std.process.Child.init(&.{
            "nix",
            "build",
            "--no-link",
            "--print-out-paths",
            try std.fmt.allocPrint(allocator, "nixpkgs#{s}", .{self.app_name}),
        }, allocator);
        nix_build_proc.stdout_behavior = .Pipe;
        try nix_build_proc.spawn();
        const stdout = nix_build_proc.stdout orelse return error.NixBuildStdoutNotFound;
        var buf: [std.fs.max_path_bytes]u8 = undefined;
        const desktop_file_offset = try stdout.readAll(&buf);
        const end_offset = desktop_file_offset + self.desktop_file.len;
        buf[desktop_file_offset - 1] = std.fs.path.sep;
        @memcpy(buf[desktop_file_offset..end_offset], self.desktop_file);
        const store_path = buf[0..end_offset];

        const desktop_file = try std.fs.cwd().openFile(store_path, .{});
        defer desktop_file.close();
        const desktopapp_option = try desktopapps.parseDesktopAppFile(allocator, desktop_file.reader().any()) orelse return error.InvalidDesktopFile;
        try desktopapp_option.launch(allocator);
    }
};
