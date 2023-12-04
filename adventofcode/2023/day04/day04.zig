const std = @import("std");
const time = std.time;
const print = std.debug.print;
const split = std.mem.splitScalar;
const ArrayList = std.ArrayList;
const HashMap = std.AutoHashMap;
const Allocator = std.mem.Allocator;

const Card = struct {
    id: u32,
    wnums: ArrayList(u32),
    mnums: ArrayList(u32),

    const Self = @This();

    fn parse(line: []const u8, allocator: Allocator) !Self {
        var cit = split(u8, line, ':');
        const ids = cit.next().?;
        const id = std.fmt.parseInt(u32, std.mem.trim(u8, ids[5..], " "), 10) catch 1;
        const nums = std.mem.trim(u8, cit.next().?, " ");

        var nit = split(u8, std.mem.trim(u8, nums, " "), '|');
        const wnumstr = std.mem.trim(u8, nit.next().?, " ");
        const mnumstr = std.mem.trim(u8, nit.next().?, " ");

        var wnums = ArrayList(u32).init(allocator);
        var wit = split(u8, wnumstr, ' ');
        while (wit.next()) |wnum| {
            if (std.mem.eql(u8, wnum, "")) continue;
            const num = std.fmt.parseInt(u32, std.mem.trim(u8, wnum, " "), 10) catch 0;
            try wnums.append(num);
        }

        var mnums = ArrayList(u32).init(allocator);
        var mit = split(u8, mnumstr, ' ');
        while (mit.next()) |mnum| {
            if (std.mem.eql(u8, mnum, "")) continue;
            const num = std.fmt.parseInt(u32, std.mem.trim(u8, mnum, " "), 10) catch 0;
            try mnums.append(num);
        }

        return Self{ .id = id, .wnums = wnums, .mnums = mnums };
    }

    fn won(self: Self) u32 {
        var r: u32 = 0;
        for (self.mnums.items) |mnum| {
            for (self.wnums.items) |wnum| {
                if (wnum == mnum) {
                    r += 1;
                    break;
                }
            }
        }
        return r;
    }

    fn score(self: Self) u32 {
        const win = self.won();
        if (win == 0) return 0;

        return std.math.pow(u32, 2, win - 1);
    }
};

fn play(cards: ArrayList(Card), allocator: Allocator) !u32 {
    var queue = ArrayList(u32).init(allocator);
    defer queue.deinit();
    var chash = HashMap(u32, Card).init(allocator);
    defer chash.deinit();
    var whash = HashMap(u32, u32).init(allocator);
    defer whash.deinit();
    for (cards.items) |card| {
        // print("card: {any}\n", .{card});
        try queue.append(card.id);
        try chash.put(card.id, card);
        try whash.put(card.id, card.won());
    }
    var r: u32 = 0;
    while (queue.items.len != 0) {
        r += 1;
        const top = queue.pop();
        const win = whash.get(top).?;
        const tcard = chash.get(top).?;
        for ((tcard.id + 1)..(tcard.id + 1 + win)) |i| {
            try queue.append(@intCast(i));
        }
    }
    return r;
}

pub fn main() !void {
    const s = time.microTimestamp();
    const stdin = std.io.getStdIn().reader();
    const allocator = std.heap.page_allocator;

    var cards = ArrayList(Card).init(allocator);
    defer cards.deinit();

    while (true) {
        var buf: [200]u8 = undefined;
        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null) break;

        const card = try Card.parse(line.?, std.heap.page_allocator);
        try cards.append(card);
    }

    var p1: u32 = 0;
    for (cards.items) |card| {
        p1 += card.score();
    }
    print("{d}\n", .{p1});
    const p2 = try play(cards, std.heap.page_allocator);
    print("{d}\n", .{p2});

    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
