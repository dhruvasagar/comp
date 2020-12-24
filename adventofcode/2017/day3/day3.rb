# frozen_string_literal: true

require 'math'

def part1(num)
  sq = Math.sqrt(num).round
  if sq.odd?
    (sq**2 - num).abs
  else
    sq - 1
  end
end

def part2(num)
  x, y = 0, 0
end

p part1(gets.to_i)
