const std = @import("std");
const time = std.time;
const print = std.debug.print;

const p1mset = Set{ .red = 12, .green = 13, .blue = 14 };
const Set = struct {
    red: u16,
    blue: u16,
    green: u16,

    const Self = @This();

    fn possible(self: Self) bool {
        return self.red <= p1mset.red and self.green <= p1mset.green and self.blue <= p1mset.blue;
    }

    fn parse(s: []const u8) Self {
        var it = std.mem.splitScalar(u8, s, ',');
        var r: u16 = 0;
        var g: u16 = 0;
        var b: u16 = 0;
        while (it.next()) |sc| {
            var scit = std.mem.splitScalar(u8, std.mem.trim(u8, sc, " "), ' ');
            const n = std.fmt.parseInt(u16, scit.next().?, 10) catch 0;
            const c = scit.next().?;
            if (std.mem.eql(u8, c, "red")) r = n;
            if (std.mem.eql(u8, c, "blue")) b = n;
            if (std.mem.eql(u8, c, "green")) g = n;
        }
        return Self{ .red = r, .blue = b, .green = g };
    }

    fn max(self: *Self, s: Self) void {
        self.red = if (self.red > s.red) self.red else s.red;
        self.blue = if (self.blue > s.blue) self.blue else s.blue;
        self.green = if (self.green > s.green) self.green else s.green;
    }

    fn score(self: Self) u32 {
        return self.red * self.blue * self.green;
    }
};

pub fn main() !void {
    const s = time.microTimestamp();
    const stdin = std.io.getStdIn().reader();

    var p1: u16 = 0;
    var p2: u32 = 0;
    while (true) {
        var buf: [200]u8 = undefined;
        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null) break;

        const l = line.?;
        var it = std.mem.splitScalar(u8, l, ':');
        const gid = std.fmt.parseInt(u16, it.next().?[5..], 10) catch 0;
        var sit = std.mem.splitScalar(u8, it.next().?, ';');
        var gset = Set{ .red = 0, .blue = 0, .green = 0 };
        var possible = true;
        while (sit.next()) |ss| {
            const set = Set.parse(ss);
            gset.max(set);
            if (!set.possible()) possible = false;
        }
        if (possible) p1 += gid;
        p2 += gset.score();
    }

    print("{d}\n", .{p1});
    print("{d}\n", .{p2});

    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
