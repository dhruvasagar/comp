# frozen_string_literal: true

def read_input
  ARGF.readlines
end

STRENGTHS = [ '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A' ]

def score(hand)
  mem = Hash.new(1)
  hand.chars.reduce(0) { |s, h|
    mem[h] += 1
    s += mem[h]
  }
end

def smap(hand, order)
  hand.chars.map {|hi| order.index(hi)}
end

def rank(hands)
  hands.sort {|a, b|
    ahand = a.split.first
    bhand = b.split.first
    as = score(ahand)
    bs = score(bhand)
    next smap(ahand, STRENGTHS) <=> smap(bhand, STRENGTHS) if as == bs

    as <=> bs
  }
end

def part1(lines)
  hands = rank(lines)
  hands.each_index.reduce(0) {|score, index|
    hand = hands[index]
    bet = hand.split.last.to_i
    score + (bet * (index + 1))
  }
end

STRENGTHS2 = [ 'J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A' ]

def best(hand)
  if hand.chars.any? {|c| c == 'J'}
    mem = hand
      .chars
      .reject {|c| c == 'J'}
      .reduce(Hash.new(0)) {|h, c| h[c] += 1; h}
    bestc = if mem.values.max == 1
      hand.chars.sort_by {|c| STRENGTHS2.index(c)}.last
    else
      hand.chars.sort_by {|c| mem[c]}.last
    end
    return hand.gsub('J', bestc)
  end
  hand
end

def rank2(hands)
  hands.sort {|a, b|
    ahand = a.split.first
    bhand = b.split.first
    as = score(best(ahand))
    bs = score(best(bhand))
    next smap(ahand, STRENGTHS2) <=> smap(bhand, STRENGTHS2) if as == bs

    as <=> bs
  }
end

def part2(lines)
  hands = rank2(lines)
  hands.each_index.reduce(0) {|score, index|
    hand = hands[index]
    bet = hand.split.last.to_i
    score + (bet * (index + 1))
  }
end

s = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
lines = read_input
p part1(lines)
p part2(lines)
e = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
puts "Time taken: #{(e - s) / 1000000}ms"
