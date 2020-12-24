# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def part1(nums)
  nums.sum
end

def part2(nums)
  freq = 0
  freqs = {}
  nums.cycle.each do |num|
    freq += num
    break if freqs[freq]

    freqs[freq] = true
  end
  freq
end

nums = read_input.map(&:to_i)
p part1(nums)
p part2(nums)
