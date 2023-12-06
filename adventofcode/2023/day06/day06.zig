const std = @import("std");
const time = std.time;
const print = std.debug.print;
const split = std.mem.splitScalar;
const trim = std.mem.trim;
const ArrayList = std.ArrayList;

fn count_wins(rtime: u64, rdist: u64) u64 {
    const l = for (0..rtime) |t| {
        if (rdist < (t * (rtime - t))) break t;
    } else 0;
    return @intCast(1 + rtime - 2 * l);
}

fn part1(input: [100]u8) !u64 {
    var lit = split(u8, &input, '\n');
    var tit = split(u8, lit.next().?, ':');
    _ = tit.next();
    var tits = split(u8, trim(u8, tit.next().?, " "), ' ');
    var times = ArrayList(u64).init(std.heap.page_allocator);
    while (tits.next()) |ns| {
        const tns = trim(u8, ns, " ");
        if (std.mem.eql(u8, tns, "")) continue;

        const n = std.fmt.parseInt(u64, tns, 10) catch 0;
        try times.append(n);
    }

    var dit = split(u8, lit.next().?, ':');
    _ = dit.next();
    var dits = split(u8, trim(u8, dit.next().?, " "), ' ');
    var dists = ArrayList(u64).init(std.heap.page_allocator);
    while (dits.next()) |ns| {
        const tns = trim(u8, ns, " ");
        if (std.mem.eql(u8, tns, "")) continue;

        const n = std.fmt.parseInt(u64, tns, 10) catch 0;
        try dists.append(n);
    }

    var w: u64 = 1;
    for (0..times.items.len) |i| {
        w *= count_wins(times.items[i], dists.items[i]);
    }
    return w;
}

fn part2(input: [100]u8) !u64 {
    var lit = split(u8, &input, '\n');
    var tit = split(u8, lit.next().?, ':');
    _ = tit.next();
    const tits = trim(u8, tit.next().?, " ");
    var tnumstr = ArrayList(u8).init(std.heap.page_allocator);
    for (tits) |c| {
        if (c == ' ') continue;

        try tnumstr.append(c);
    }
    const tnums = try tnumstr.toOwnedSlice();
    const tnum = std.fmt.parseInt(u64, tnums, 10) catch 0;

    var dit = split(u8, lit.next().?, ':');
    _ = dit.next();
    const dits = trim(u8, dit.next().?, " ");
    var dnumstr = ArrayList(u8).init(std.heap.page_allocator);
    for (dits) |c| {
        if (c == ' ') continue;

        try dnumstr.append(c);
    }
    const dnums = try dnumstr.toOwnedSlice();
    const dnum = std.fmt.parseInt(u64, dnums, 10) catch 0;
    return count_wins(tnum, dnum);
}

pub fn main() !void {
    const s = time.microTimestamp();

    var lines: [100]u8 = undefined;
    _ = try std.io.getStdIn().readAll(&lines);

    const p1 = try part1(lines);
    print("{d}\n", .{p1});

    const p2 = try part2(lines);
    print("{d}\n", .{p2});

    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
