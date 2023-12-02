const std = @import("std");
const time = std.time;
const print = std.debug.print;

const Range = struct {
    low: u16,
    high: u16,

    fn contains(self: Range, r: Range) bool {
        return (self.low <= r.low and self.high >= r.high) or (r.low <= self.low and r.high >= self.high);
    }

    fn overlap(self: Range, r: Range) bool {
        return (self.low <= r.low and self.high >= r.low) or (self.low >= r.low and self.low <= r.high);
    }
};

fn parseRange(rs: []const u8) Range {
    var it = std.mem.splitScalar(u8, rs, '-');
    return .{
        .low = std.fmt.parseInt(u16, it.next().?, 10) catch 0,
        .high = std.fmt.parseInt(u16, it.next().?, 10) catch 0,
    };
}

fn parseRangeTuple(rs: []u8) std.meta.Tuple(&.{ Range, Range }) {
    var it = std.mem.splitScalar(u8, rs, ',');
    return .{ parseRange(it.next().?), parseRange(it.next().?) };
}

pub fn main() !void {
    const s = time.microTimestamp();
    const stdin = std.io.getStdIn().reader();
    var p1: u16 = 0;
    var p2: u16 = 0;
    while (true) {
        var buf: [100]u8 = undefined;
        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null) break;

        const rt = parseRangeTuple(line.?);
        const r1 = rt[0];
        const r2 = rt[1];
        if (r1.contains(r2)) p1 += 1;
        if (r1.overlap(r2)) p2 += 1;
    }
    print("{d}\n", .{p1});
    print("{d}\n", .{p2});
    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
