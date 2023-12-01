const std = @import("std");
const time = std.time;

pub fn main() !void {
    const s = time.microTimestamp();
    const s1 = time.microTimestamp();

    const allocator = std.heap.page_allocator;

    const stdin = std.io.getStdIn().reader();
    var buffer: [256]u8 = undefined;
    var numbers = std.ArrayList(u32).init(allocator);
    defer numbers.deinit();

    var tot: u32 = 0;
    while (true) {
        const chunk = try stdin.readUntilDelimiterOrEof(&buffer, '\n');
        if (chunk == null) break;
        if (std.mem.eql(u8, chunk.?, "")) {
            try numbers.append(tot);
            tot = 0;
        }
        const ni = std.fmt.parseInt(u32, chunk.?, 10) catch 0;
        tot += ni;
    }
    const nums = try numbers.toOwnedSlice();
    std.sort.heap(u32, nums, {}, std.sort.desc(u32));

    // part1
    std.debug.print("{d}\n", .{nums[0]});
    const e1 = time.microTimestamp();

    // part2
    const s2 = time.microTimestamp();
    std.debug.print("{d}\n", .{nums[0] + nums[1] + nums[2]});
    const e2 = time.microTimestamp();

    const e = time.microTimestamp();
    std.debug.print("Time for part1: {s}, part2: {s}, total: {s}\n", .{ std.fmt.fmtDurationSigned(e1 - s1), std.fmt.fmtDurationSigned(e2 - s2), std.fmt.fmtDurationSigned(e - s) });
}
