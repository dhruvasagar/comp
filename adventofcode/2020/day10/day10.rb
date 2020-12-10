# frozen_string_literal: true

def read_input
  ARGF.readlines
end

INPUT_RANGE = 1..3
OUTLET_JOLT = 0
DEVICE_JOlTAGE_DIFF = 3

def part1(joltages)
  diff1 = 0
  diff3 = 1
  sjolt = OUTLET_JOLT
  joltages.each do |jolt|
    diff = jolt - sjolt
    if diff == 1
      diff1 += 1
    elsif diff == 3
      diff3 += 1
    end
    sjolt = jolt
  end
  diff1 * diff3
end

def part2(jolts)
  # tribonacci sequence (sum of previous 3) applied to only values that
  # exist in jolts
  h = { 0 => 1 }
  jolts.sort.each do |j|
    h[j] = ((j - 3)...j).map { |jp| h[jp] }.compact.sum
  end
  p h
  h.values.last
end

joltages = read_input.map(&:to_i).sort
p part1(joltages)
p part2(joltages)
