# frozen_string_literal: true

def valid?(trs)
  a, b, c = trs.sort
  a + b > c
end

def part1(tsides)
  tsides.select { |ts| valid?(ts) }.size
end

def part2(tsides)
  tsides.each_slice(3).map do |ts|
    ts.transpose.select { |tss| valid?(tss) }.size
  end.sum
end

def read_input
  ARGF.readlines
end

tsides = read_input.map(&:split).map { |ts| ts.map(&:to_i) }
p part1(tsides)
p part2(tsides)
