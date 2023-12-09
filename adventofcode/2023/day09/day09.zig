const std = @import("std");
const time = std.time;
const print = std.debug.print;
const ArrayList = std.ArrayList(i32);

fn diffs(nums: ArrayList) ArrayList {
    var res = ArrayList.init(std.heap.page_allocator);
    for (1..nums.items.len) |i| {
        res.append(nums.items[i] - nums.items[i - 1]) catch unreachable;
    }
    return res;
}

fn predictNext(nums: ArrayList) i32 {
    const ndiffs = diffs(nums);
    const allZeroes = for (ndiffs.items) |ndiff| {
        if (ndiff != 0) break false;
    } else true;
    if (allZeroes) return nums.items[0];

    return nums.items[nums.items.len - 1] + predictNext(ndiffs);
}

fn predictPrev(nums: ArrayList) i32 {
    const ndiffs = diffs(nums);
    const areZeroes = for (ndiffs.items) |ndiff| {
        if (ndiff != 0) break false;
    } else true;
    if (areZeroes) return nums.items[0];

    return nums.items[0] - predictPrev(ndiffs);
}

pub fn main() !void {
    const s = time.microTimestamp();
    const stdin = std.io.getStdIn().reader();
    var p1: i32 = 0;
    var p2: i32 = 0;
    while (true) {
        var buf: [300]u8 = undefined;
        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null) break;

        const l = line.?;
        var nit = std.mem.splitScalar(u8, l, ' ');
        var nums = ArrayList.init(std.heap.page_allocator);
        while (nit.next()) |ns| {
            if (!std.ascii.isASCII(ns[0])) break;

            const n = try std.fmt.parseInt(i32, ns, 10);
            try nums.append(n);
        }
        p1 += predictNext(nums);
        p2 += predictPrev(nums);
    }
    print("{d}\n{d}\n", .{ p1, p2 });
    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
