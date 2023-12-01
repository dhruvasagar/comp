const std = @import("std");
const time = std.time;
const print = std.debug.print;

fn priority(a: u8) u8 {
    if (std.ascii.isLower(a)) {
        return a - 'a' + 1;
    } else {
        return a - 'A' + 27;
    }
}

fn find_common(buf: []u8) u8 {
    const mid: usize = buf.len / 2;
    for (0..mid) |i| {
        for (mid..buf.len) |j| {
            if (buf[i] == buf[j]) return buf[i];
        }
    }
    return 0;
}

fn find_common_multi(bufs: [][100]u8) u8 {
    for (0..100) |i| {
        for (0..100) |j| {
            for (0..100) |k| {
                if (bufs[0][i] == bufs[1][j] and bufs[0][i] == bufs[2][k]) return bufs[0][i];
            }
        }
    }
    return 0;
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const s = time.microTimestamp();
    const stdin = std.io.getStdIn().reader();

    var batches = std.ArrayList([100]u8).init(allocator);
    defer batches.deinit();

    var p1: u16 = 0;
    var p2: u16 = 0;
    var bi: usize = 0;
    const batch_size: usize = 3;
    while (true) {
        var buf: [100]u8 = undefined;
        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null) break;

        p1 += priority(find_common(line.?));
        try batches.append(buf);

        bi += 1;
        if (bi % batch_size == 0) {
            p2 += priority(find_common_multi(batches.items[(bi - batch_size)..bi]));
        }
    }
    print("{d}\n", .{p1});
    print("{d}\n", .{p2});

    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
