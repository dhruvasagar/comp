const std = @import("std");
const time = std.time;
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

fn Set(comptime K: type) type {
    return std.AutoHashMap(K, void);
}

const Pos = struct {
    x: i32,
    y: i32,
};

const Dir = struct {
    dx: i32,
    dy: i32,

    const Self = @This();

    fn isBackward(self: Self, a: Dir) bool {
        if (self.dx == 0) return self.dy == (-a.dy);
        if (self.dy == 0) return self.dx == (-a.dx);
        return false;
    }
};

const DIRS = [4]Dir{
    Dir{ .dx = 1, .dy = 0 },
    Dir{ .dx = 0, .dy = -1 },
    Dir{ .dx = -1, .dy = 0 },
    Dir{ .dx = 0, .dy = 1 },
};

const Node = struct {
    cost: i32,
    dirIndex: usize,
    blockSize: i32,
    pos: Pos,

    const Self = @This();

    fn lessThan(context: void, a: Self, b: Self) std.math.Order {
        _ = context;
        return std.math.order(a.cost, b.cost);
    }
};

const VisitedNode = struct {
    dirIndex: usize,
    blockSize: i32,
    pos: Pos,
};

const Grid = struct {
    ymax: usize,
    xmax: usize,
    grid: ArrayList(ArrayList(i32)),

    allocator: Allocator,

    const Self = @This();

    fn init(grid: ArrayList(ArrayList(i32)), allocator: Allocator) Self {
        const ymax = grid.items.len;
        const xmax = grid.items[0].items.len;
        return Self{ .ymax = ymax, .xmax = xmax, .grid = grid, .allocator = allocator };
    }

    fn outside(self: Self, pos: Pos) bool {
        return pos.x < 0 or pos.y < 0 or pos.x >= self.xmax or pos.y >= self.ymax;
    }

    fn findPath(self: Self, min: i32, max: i32) i32 {
        var visited = Set(VisitedNode).init(self.allocator);
        defer visited.deinit();
        visited.put(.{ .dirIndex = 0, .blockSize = 0, .pos = Pos{ .x = 0, .y = 0 } }, {}) catch unreachable;
        var queue = std.PriorityQueue(Node, void, Node.lessThan).init(self.allocator, {});
        defer queue.deinit();
        queue.add(.{ .cost = 0, .dirIndex = 0, .blockSize = 0, .pos = Pos{ .x = 0, .y = 0 } }) catch unreachable;
        const target = Pos{ .x = @intCast(self.xmax - 1), .y = @intCast(self.ymax - 1) };
        while (queue.count() > 0) {
            const node = queue.remove();
            // print("node: {any}\n", .{node});
            for (0..4) |i| {
                if (i == node.dirIndex and node.blockSize == max) continue;
                if (i != node.dirIndex and node.blockSize < min) continue;

                var blockSize: i32 = 1;
                if (i == node.dirIndex) blockSize = node.blockSize + 1;

                const dir = DIRS[i];
                const npos: Pos = Pos{ .x = node.pos.x + dir.dx, .y = node.pos.y + dir.dy };
                if (self.outside(npos) or dir.isBackward(DIRS[@intCast(node.dirIndex)])) continue;

                const ncost: i32 = node.cost + self.grid.items[@intCast(npos.y)].items[@intCast(npos.x)];
                if (std.meta.eql(npos, target)) return ncost;

                if (visited.contains(.{ .dirIndex = i, .blockSize = blockSize, .pos = npos })) continue;
                visited.put(.{ .dirIndex = i, .blockSize = blockSize, .pos = npos }, {}) catch unreachable;

                queue.add(.{ .cost = ncost, .dirIndex = i, .blockSize = blockSize, .pos = npos }) catch unreachable;
            }
        }
        return 0;
    }
};

pub fn main() !void {
    const s = time.microTimestamp();
    const stdin = std.io.getStdIn().reader();
    var gnums = ArrayList(ArrayList(i32)).init(std.heap.page_allocator);
    defer gnums.deinit();
    while (true) {
        var buf: [150]u8 = undefined;
        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null) break;

        const l = line.?;
        var nums = ArrayList(i32).init(std.heap.page_allocator);
        for (l) |c| {
            if (!std.ascii.isASCII(c)) break;

            const n: i32 = std.fmt.parseInt(i32, &[_]u8{c}, 10) catch 0;
            try nums.append(n);
        }
        try gnums.append(nums);
    }
    const grid = Grid.init(gnums, std.heap.page_allocator);
    print("{d}\n", .{grid.findPath(0, 3)});
    print("{d}\n", .{grid.findPath(4, 10)});
    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
