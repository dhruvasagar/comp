const std = @import("std");
const time = std.time;
const print = std.debug.print;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const NumArray = std.BoundedArray(u8, 5);

const CARDS = "23456789TJQKA";
const CARDS2 = "J23456789TQKA";

const Hand = struct {
    cards: [5]u8,
    bet: u16,
    reference: [13]u8,
    handleJoker: bool,

    cardCount: std.AutoHashMap(u8, u16),

    const Self = @This();

    fn init(cards: [5]u8, bet: u16, reference: [13]u8, handleJoker: bool, allocator: Allocator) Self {
        var cardCount = std.AutoHashMap(u8, u16).init(allocator);
        for (cards) |h| {
            if (cardCount.get(h)) |hc| {
                cardCount.put(h, hc + 1) catch unreachable;
            } else {
                cardCount.put(h, 1) catch unreachable;
            }
        }
        return Self{ .cards = cards, .bet = bet, .reference = reference, .handleJoker = handleJoker, .cardCount = cardCount };
    }

    fn score(self: Self) u16 {
        var s: u16 = 0;
        for (self.cards) |h| {
            const hc = self.cardCount.get(h).?;
            s += hc;
        }
        return s;
    }

    fn lessThan(_: void, a: Self, b: Self) bool {
        const as = if (a.handleJoker) a.best().score() else a.score();
        const bs = if (b.handleJoker) b.best().score() else b.score();
        if (as == bs) {
            for (0..5) |i| {
                if (a.cards[i] == b.cards[i]) continue;

                const iOfA = std.mem.indexOfScalar(u8, &a.reference, a.cards[i]).?;
                const iOfB = std.mem.indexOfScalar(u8, &b.reference, b.cards[i]).?;
                return iOfA < iOfB;
            }
        }
        return as < bs;
    }

    fn best(self: Self) Self {
        const index = std.mem.indexOfScalar(u8, &self.cards, 'J');
        if (index == null) return self;

        var cardCountMax: u16 = 0;
        var cardCount = std.AutoHashMap(u8, u16).init(std.heap.page_allocator);
        for (self.cards) |h| {
            if (cardCount.get(h)) |hc| {
                cardCount.put(h, hc + 1) catch unreachable;
            } else {
                cardCount.put(h, 1) catch unreachable;
            }
        }
        if (self.handleJoker) {
            cardCount.put('J', 0) catch unreachable;
        }
        var cit = cardCount.valueIterator();
        while (cit.next()) |v| {
            if (cardCountMax < v.*) cardCountMax = v.*;
        }

        var max_card: u8 = self.cards[0];
        for (0..5) |i| {
            if (cardCountMax == 1) {
                const iOfI = std.mem.indexOfScalar(u8, &self.reference, self.cards[i]).?;
                const iOfMax = std.mem.indexOfScalar(u8, &self.reference, max_card).?;
                if (iOfI > iOfMax) {
                    max_card = self.cards[i];
                }
            } else {
                const cci = cardCount.get(self.cards[i]).?;
                const ccm = cardCount.get(max_card).?;
                if (cci > ccm) {
                    max_card = self.cards[i];
                }
            }
        }
        var shand: [5]u8 = std.mem.zeroes([5]u8);
        for (0..5) |i| {
            shand[i] = if (self.cards[i] == 'J') max_card else self.cards[i];
        }
        return Self.init(shand, self.bet, self.reference, self.handleJoker, std.heap.page_allocator);
    }

    fn winnings(self: Self, rank: u16) u64 {
        const bet: u64 = @intCast(self.bet);
        return bet * (rank + 1);
    }
};

fn parseHands(input: []const u8, reference: [13]u8, handleJoker: bool) []Hand {
    var it = std.mem.splitScalar(u8, input, '\n');
    var hands = std.BoundedArray(Hand, 1000).init(0) catch unreachable;
    while (it.next()) |line| {
        if (!std.ascii.isASCII(line[0])) break;

        var hbetit = std.mem.splitScalar(u8, line, ' ');
        var cards: [5]u8 = std.mem.zeroes([5]u8);
        const cs = hbetit.next().?;
        for (0..5) |i| {
            cards[i] = cs[i];
        }
        const bet = std.fmt.parseInt(u16, hbetit.next().?, 10) catch 0;
        const hand = Hand.init(cards, bet, reference, handleJoker, std.heap.page_allocator);
        hands.append(hand) catch unreachable;
    }
    return hands.slice();
}

fn part1(input: []const u8) u64 {
    var hands = parseHands(input, CARDS.*, false);
    std.sort.heap(Hand, hands, {}, Hand.lessThan);
    var tot_winnings: u64 = 0;
    for (0..hands.len) |i| {
        tot_winnings += hands[i].winnings(@intCast(i));
    }
    return tot_winnings;
}

fn part2(input: []const u8) u64 {
    var hands = parseHands(input, CARDS2.*, true);
    std.sort.heap(Hand, hands, {}, Hand.lessThan);
    var tot_winnings: u64 = 0;
    for (0..hands.len) |i| {
        tot_winnings += hands[i].winnings(@intCast(i));
    }
    return tot_winnings;
}

pub fn main() !void {
    const s = time.microTimestamp();
    const stdin = std.io.getStdIn().reader();
    var buf: [10000]u8 = undefined;
    _ = try stdin.readAll(&buf);
    print("{d}\n", .{part1(&buf)});
    print("{d}\n", .{part2(&buf)});
    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
