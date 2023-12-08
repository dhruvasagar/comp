const std = @import("std");
const time = std.time;
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const KeyArray = std.BoundedArray([3]u8, 1000);

fn gcd(a: u64, b: u64) u64 {
    if (b == 0) {
        return a;
    }
    return gcd(b, @mod(a, b));
}

fn lcm(a: u64, b: u64) u64 {
    if (a > b) {
        return (a / gcd(a, b)) * b;
    }
    return (b / gcd(a, b)) * a;
}

const Pair = struct {
    left: [3]u8,
    right: [3]u8,
};

const HashMap = std.AutoHashMap([3]u8, Pair);

const DestMap = struct {
    map: HashMap,
    steps: []const u8,

    const Self = @This();

    fn parse(input: []const u8, allocator: Allocator) Self {
        var map = HashMap.init(allocator);
        var it = std.mem.splitScalar(u8, input, '\n');
        const steps = it.next().?;
        _ = it.next();
        while (it.next()) |line| {
            if (!std.ascii.isASCII(line[0])) break;

            var pit = std.mem.splitScalar(u8, line, '=');

            const keystr = std.mem.trim(u8, pit.next().?, " ");
            var key: [3]u8 = std.mem.zeroes([3]u8);
            for (0..3) |i| {
                key[i] = keystr[i];
            }

            var dit = std.mem.splitScalar(u8, std.mem.trim(u8, pit.next().?, " "), ',');
            const leftstr = std.mem.trim(u8, dit.next().?, " ");
            const rightstr = std.mem.trim(u8, dit.next().?, " ");

            var left: [3]u8 = std.mem.zeroes([3]u8);
            var right: [3]u8 = std.mem.zeroes([3]u8);
            for (0..3) |i| {
                left[i] = leftstr[i + 1];
                right[i] = rightstr[i];
            }

            const pair = Pair{ .left = left, .right = right };
            map.put(key, pair) catch unreachable;
        }
        return Self{ .map = map, .steps = steps };
    }

    fn part1(self: Self) u64 {
        const end: [3]u8 = [3]u8{ 'Z', 'Z', 'Z' };
        var start: [3]u8 = [3]u8{ 'A', 'A', 'A' };
        var s: u64 = 0;
        return while (true) {
            if (std.mem.eql(u8, &start, &end)) {
                break s;
            }

            const step = self.steps[@mod(s, self.steps.len)];
            const next = self.map.get(start).?;
            start = if (step == 'L') next.left else next.right;
            s += 1;
        };
    }

    fn part2(self: Self) u64 {
        var start_nodes = KeyArray.init(0) catch unreachable;
        var it = self.map.keyIterator();
        while (it.next()) |key| {
            if (std.mem.endsWith(u8, key, "A")) {
                start_nodes.append(key.*) catch unreachable;
            }
        }
        const starts = start_nodes.slice();
        var counts = std.BoundedArray(u64, 1000).init(0) catch unreachable;
        for (starts) |n| {
            var start = n;
            var s: u64 = 0;
            const count = while (true) {
                if (std.mem.endsWith(u8, &start, "Z")) {
                    break s;
                }

                const step = self.steps[@mod(s, self.steps.len)];
                const next = self.map.get(start).?;
                start = if (step == 'L') next.left else next.right;
                s += 1;
            };
            counts.append(count) catch unreachable;
        }
        var tlcm: u64 = 1;
        for (counts.slice()) |c| {
            tlcm = lcm(tlcm, c);
        }
        return tlcm;
    }
};

pub fn main() !void {
    const s = time.microTimestamp();

    var input: [100000]u8 = undefined;
    _ = try std.io.getStdIn().readAll(&input);

    const destMap = DestMap.parse(&input, std.heap.page_allocator);
    print("{d}\n", .{destMap.part1()});
    print("{d}\n", .{destMap.part2()});

    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
