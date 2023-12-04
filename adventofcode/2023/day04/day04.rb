# frozen_string_literal: true

def read_input
  ARGF.readlines
end

class Card
  attr_accessor :idx, :wnums, :mnums

  def initialize(idx, wnums, mnums)
    @idx = idx
    @wnums = wnums
    @mnums = mnums
  end

  def won
    @won ||= (mnums & wnums).size
  end

  def score
    return 0 if won.zero?

    2 ** (won - 1)
  end

  def to_s
    "Card #{idx}: #{wnums} | #{mnums}"
  end
  alias :inspect :to_s
end

def parse_card(line)
  card, nums = line.split(":")
  win_nums, my_nums = nums.split("|")
  Card.new(card.split[-1].to_i, win_nums.split, my_nums.split)
end

s = Process.clock_gettime(Process::CLOCK_MONOTONIC)
s1 = Process.clock_gettime(Process::CLOCK_MONOTONIC)

lines = read_input
cards = lines.map {|line| parse_card(line)}

e1 = Process.clock_gettime(Process::CLOCK_MONOTONIC)

# Part 1
p cards.map {|c| c.score}.sum

def play(cards)
  processed = cards.inject({}) {|h, c| h[c.idx-1] = 0; h}
  unprocessed = cards.inject({}) {|h, c| h[c.idx-1] = 1; h}
  while unprocessed.values.sum != 0
    cards.each.with_index do |card, index|
      # p unprocessed
      next if unprocessed[index].zero?

      processed[index] += 1
      unprocessed[index] -= 1
      won = card.won
      ((index+1)...(index+1+won)).each do |i|
        unprocessed[i] += 1
      end
    end
  end
  processed.values.sum
end

s2 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
pp play(cards)
e2 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
e = Process.clock_gettime(Process::CLOCK_MONOTONIC)

puts "Time for part1: #{(e1-s1) * 1000}ms, part2: #{(e2-s2) * 1000}ms, total: #{(e-s) * 1000}ms"
