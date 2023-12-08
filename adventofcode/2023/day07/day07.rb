# frozen_string_literal: true

def read_input
  ARGF.readlines
end

CARDS = [ '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A' ]
CARDS_2 = [ 'J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A' ]

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

def best(hand)
  return hand unless hand.chars.any? {|c| c == 'J'}

  mem = hand
    .chars
    .reject {|c| c == 'J'}
    .reduce(Hash.new(0)) {|h, c| h[c] += 1; h}
  memmax = mem.values.max
  bestc = hand.chars.sort_by {|c| memmax == 1 ? CARDS_2.index(c) : mem[c]}.last
  hand.gsub('J', bestc)
end

def rank(lines, order = CARDS, joker = false)
  lines.sort {|a, b|
    ahand = a.split.first
    bhand = b.split.first
    as = score(joker ? best(ahand) : ahand)
    bs = score(joker ? best(bhand) : bhand)
    next smap(ahand, order) <=> smap(bhand, order) if as == bs

    as <=> bs
  }
end

def total_winnings(lines)
  lines.each_index.reduce(0) {|score, index|
    hand = lines[index]
    bet = hand.split.last.to_i
    score + (bet * (index + 1))
  }
end

def part1(lines)
  total_winnings(rank(lines))
end

def part2(lines)
  total_winnings(rank(lines, CARDS_2, true))
end

s = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
lines = read_input
p part1(lines)
p part2(lines)
e = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
puts "Time taken: #{(e - s) / 1000000}ms"
