const std = @import("std");
const time = std.time;
const print = std.debug.print;
const toDigit = std.fmt.charToDigit;

fn first_digit(ns: []u8) u8 {
    for (ns) |n| {
        if (std.ascii.isDigit(n)) return toDigit(n, 10) catch 0;
    }
    return 0;
}

fn last_digit(ns: []u8) u8 {
    for (0..ns.len) |i| {
        if (std.ascii.isDigit(ns[ns.len - i - 1])) return toDigit(ns[ns.len - i - 1], 10) catch 0;
    }
    return 0;
}

const one = "one";
const two = "two";
const three = "three";
const four = "four";
const five = "five";
const six = "six";
const seven = "seven";
const eight = "eight";
const nine = "nine";

fn first_digit2(ns: []u8) u8 {
    const l = ns.len;
    for (0..l) |i| {
        if (i + 3 < l and std.mem.eql(u8, one, ns[i .. i + 3])) return 1;
        if (i + 3 < l and std.mem.eql(u8, two, ns[i .. i + 3])) return 2;
        if (i + 5 < l and std.mem.eql(u8, three, ns[i .. i + 5])) return 3;
        if (i + 4 < l and std.mem.eql(u8, four, ns[i .. i + 4])) return 4;
        if (i + 4 < l and std.mem.eql(u8, five, ns[i .. i + 4])) return 5;
        if (i + 3 < l and std.mem.eql(u8, six, ns[i .. i + 3])) return 6;
        if (i + 5 < l and std.mem.eql(u8, seven, ns[i .. i + 5])) return 7;
        if (i + 5 < l and std.mem.eql(u8, eight, ns[i .. i + 5])) return 8;
        if (i + 4 < l and std.mem.eql(u8, nine, ns[i .. i + 4])) return 9;
        if (std.ascii.isDigit(ns[i])) return toDigit(ns[i], 10) catch 0;
    }
    return 0;
}

fn last_digit2(ns: []u8) u8 {
    var i: usize = ns.len;
    while (i > 0) {
        i -= 1;
        if (i >= 2 and std.mem.eql(u8, one, ns[(i - 2)..(i + 1)])) return 1;
        if (i >= 2 and std.mem.eql(u8, two, ns[(i - 2)..(i + 1)])) return 2;
        if (i >= 4 and std.mem.eql(u8, three, ns[(i - 4)..(i + 1)])) return 3;
        if (i >= 3 and std.mem.eql(u8, four, ns[(i - 3)..(i + 1)])) return 4;
        if (i >= 3 and std.mem.eql(u8, five, ns[(i - 3)..(i + 1)])) return 5;
        if (i >= 2 and std.mem.eql(u8, six, ns[(i - 2)..(i + 1)])) return 6;
        if (i >= 4 and std.mem.eql(u8, seven, ns[(i - 4)..(i + 1)])) return 7;
        if (i >= 4 and std.mem.eql(u8, eight, ns[(i - 4)..(i + 1)])) return 8;
        if (i >= 3 and std.mem.eql(u8, nine, ns[(i - 3)..(i + 1)])) return 9;
        if (std.ascii.isDigit(ns[i])) return toDigit(ns[i], 10) catch 0;
    }
    return 0;
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

        const l = line.?;
        p1 += 10 * first_digit(l) + last_digit(l);
        p2 += 10 * first_digit2(l) + last_digit2(l);
    }
    print("{d}\n", .{p1});
    print("{d}\n", .{p2});
    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
