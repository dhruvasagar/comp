const std = @import("std");
const time = std.time;

const Move = enum {
    rock,
    paper,
    scissor,
};

fn parseMove(a: u8) Move {
    return switch (a) {
        'A', 'X' => Move.rock,
        'B', 'Y' => Move.paper,
        'C', 'Z' => Move.scissor,
        else => Move.rock,
    };
}

const Outcome = enum {
    win,
    draw,
    lose,
};

fn parseOutcome(o: u8) Outcome {
    return switch (o) {
        'X' => Outcome.lose,
        'Y' => Outcome.draw,
        'Z' => Outcome.win,
        else => Outcome.draw,
    };
}

fn parseMoveOutcome(a: u8, b: u8) [2]Move {
    const move = parseMove(a);
    return [2]Move{ move, pickMove(move, parseOutcome(b)) };
}

fn moveScore(move: Move) u8 {
    return switch (move) {
        .rock => 1,
        .paper => 2,
        .scissor => 3,
    };
}

fn outcomeScore(out: Outcome) u8 {
    return switch (out) {
        .lose => 0,
        .draw => 3,
        .win => 6,
    };
}

fn pickMove(move: Move, out: Outcome) Move {
    return switch (move) {
        .rock => {
            return switch (out) {
                .draw => Move.rock,
                .win => Move.paper,
                .lose => Move.scissor,
            };
        },
        .paper => {
            return switch (out) {
                .draw => Move.paper,
                .win => Move.scissor,
                .lose => Move.rock,
            };
        },
        .scissor => {
            return switch (out) {
                .draw => Move.scissor,
                .win => Move.rock,
                .lose => Move.paper,
            };
        },
    };
}

fn outcome(move1: Move, move2: Move) Outcome {
    return switch (move1) {
        .rock => {
            return switch (move2) {
                .rock => Outcome.draw,
                .scissor => Outcome.lose,
                .paper => Outcome.win,
            };
        },
        .paper => {
            return switch (move2) {
                .paper => Outcome.draw,
                .rock => Outcome.lose,
                .scissor => Outcome.win,
            };
        },
        .scissor => {
            return switch (move2) {
                .scissor => Outcome.draw,
                .paper => Outcome.lose,
                .rock => Outcome.win,
            };
        },
    };
}

fn roundScore(move1: Move, move2: Move) u32 {
    const out = outcome(move1, move2);
    return moveScore(move2) + outcomeScore(out);
}

pub fn main() !void {
    const s = time.microTimestamp();
    const stdin = std.io.getStdIn().reader();
    var p1: u32 = 0;
    var p2: u32 = 0;
    while (true) {
        var buf: [10]u8 = undefined;
        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null) break;

        const lines = line.?;
        if (lines.len < 3) break;

        const move1 = parseMove(lines[0]);
        const move2 = parseMove(lines[2]);
        p1 += roundScore(move1, move2);

        const moves = parseMoveOutcome(lines[0], lines[2]);
        p2 += roundScore(moves[0], moves[1]);
    }
    std.debug.print("{d}\n", .{p1});
    std.debug.print("{d}\n", .{p2});
    const e = time.microTimestamp();
    std.debug.print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
