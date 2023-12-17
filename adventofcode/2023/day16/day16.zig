const std = @import("std");
const time = std.time;
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const HashMap = std.AutoHashMap;

const Pos = struct {
    x: i32,
    y: i32,

    const Self = @This();

    fn init(x: i32, y: i32) Self {
        return Self{ .x = x, .y = y };
    }

    fn up(self: Self) Self {
        return Self{ .x = self.x, .y = self.y - 1 };
    }
    fn down(self: Self) Self {
        return Self{ .x = self.x, .y = self.y + 1 };
    }
    fn left(self: Self) Self {
        return Self{ .x = self.x - 1, .y = self.y };
    }
    fn right(self: Self) Self {
        return Self{ .x = self.x + 1, .y = self.y };
    }
};

const Direction = enum {
    up,
    down,
    left,
    right,
};

const Cell = enum {
    space,

    leftMirror,
    rightMirror,

    verticalSplitter,
    horizontalSplitter,
};

const Grid = struct {
    grid: ArrayList(ArrayList(Cell)),
    xmax: usize,
    ymax: usize,
    allocator: Allocator,

    const Self = @This();

    fn init(allocator: Allocator) Self {
        const grid = ArrayList(ArrayList(Cell)).init(allocator);
        return Self{ .grid = grid, .xmax = 0, .ymax = 0, .allocator = allocator };
    }

    fn len(self: Self) usize {
        return self.grid.items.len;
    }

    fn append(self: *Self, row: ArrayList(Cell)) !void {
        self.ymax += 1;
        self.xmax = row.items.len;
        return try self.grid.append(row);
    }

    fn deinit(self: Self) void {
        self.grid.deinit();
    }

    fn outside(self: Self, pos: Pos) bool {
        if (pos.x < 0 or pos.y < 0 or pos.x >= self.xmax or pos.y >= self.ymax) {
            return true;
        }
        return false;
    }

    fn dfs(self: *Self, pos: Pos, dirn: Direction, vis: *HashMap(Pos, Direction)) void {
        if (self.outside(pos)) return;

        const d = vis.get(pos);
        if (d != null and d.? == dirn) return;

        vis.put(pos, dirn) catch unreachable;
        const c = self.grid.items[@intCast(pos.y)].items[@intCast(pos.x)];
        switch (c) {
            .space => {
                switch (dirn) {
                    .up => self.dfs(pos.up(), dirn, vis),
                    .down => self.dfs(pos.down(), dirn, vis),
                    .left => self.dfs(pos.left(), dirn, vis),
                    .right => self.dfs(pos.right(), dirn, vis),
                }
            },
            .verticalSplitter => {
                switch (dirn) {
                    .up => self.dfs(pos.up(), dirn, vis),
                    .down => self.dfs(pos.down(), dirn, vis),
                    .left, .right => {
                        self.dfs(pos.up(), Direction.up, vis);
                        self.dfs(pos.down(), Direction.down, vis);
                    },
                }
            },
            .horizontalSplitter => {
                switch (dirn) {
                    .up, .down => {
                        self.dfs(pos.left(), Direction.left, vis);
                        self.dfs(pos.right(), Direction.right, vis);
                    },
                    .left => self.dfs(pos.left(), dirn, vis),
                    .right => self.dfs(pos.right(), dirn, vis),
                }
            },
            .leftMirror => {
                switch (dirn) {
                    .up => self.dfs(pos.left(), Direction.left, vis),
                    .down => self.dfs(pos.right(), Direction.right, vis),
                    .left => self.dfs(pos.up(), Direction.up, vis),
                    .right => self.dfs(pos.down(), Direction.down, vis),
                }
            },
            .rightMirror => {
                switch (dirn) {
                    .up => self.dfs(pos.right(), Direction.right, vis),
                    .down => self.dfs(pos.left(), Direction.left, vis),
                    .left => self.dfs(pos.down(), Direction.down, vis),
                    .right => self.dfs(pos.up(), Direction.up, vis),
                }
            },
        }
        return;
    }
};

fn part1(grid: *Grid) u32 {
    var hash = HashMap(Pos, Direction).init(grid.allocator);
    defer hash.deinit();
    grid.dfs(Pos.init(0, 0), Direction.right, &hash);
    return hash.count();
}

fn part2(grid: *Grid) u32 {
    var maxCount: u32 = 0;
    for (0..grid.xmax) |x| {
        var h = HashMap(Pos, Direction).init(grid.allocator);
        grid.dfs(Pos.init(@intCast(x), 0), Direction.down, &h);
        if (maxCount < h.count()) maxCount = h.count();
    }
    for (0..grid.xmax) |x| {
        var h = HashMap(Pos, Direction).init(grid.allocator);
        grid.dfs(Pos.init(@intCast(x), @intCast(grid.ymax - 1)), Direction.up, &h);
        if (maxCount < h.count()) maxCount = h.count();
    }
    for (0..grid.ymax) |y| {
        var h = HashMap(Pos, Direction).init(grid.allocator);
        grid.dfs(Pos.init(0, @intCast(y)), Direction.right, &h);
        if (maxCount < h.count()) maxCount = h.count();
    }
    for (0..grid.ymax) |y| {
        var h = HashMap(Pos, Direction).init(grid.allocator);
        grid.dfs(Pos.init(@intCast(grid.xmax - 1), @intCast(y)), Direction.left, &h);
        if (maxCount < h.count()) maxCount = h.count();
    }
    return maxCount;
}

pub fn main() !void {
    const s = time.microTimestamp();
    const stdin = std.io.getStdIn().reader();
    var grid = Grid.init(std.heap.page_allocator);
    defer grid.deinit();
    while (true) {
        var buf: [150]u8 = undefined;
        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null) break;

        const l = line.?;
        var row = ArrayList(Cell).init(std.heap.page_allocator);
        for (0..l.len) |i| {
            const c = l[i];
            const cell = switch (c) {
                '.' => Cell.space,
                '|' => Cell.verticalSplitter,
                '-' => Cell.horizontalSplitter,
                '\\' => Cell.leftMirror,
                '/' => Cell.rightMirror,
                else => unreachable,
            };
            try row.append(cell);
        }
        try grid.append(row);
    }
    print("{d}\n", .{part1(&grid)});
    print("{d}\n", .{part2(&grid)});
    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
