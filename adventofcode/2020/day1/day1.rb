# frozen_string_literal: true

def read_input
  ARGF.readlines
end

SUM_GOAL = 2020

def part1
  nums = read_input.map(&:to_i).sort
  low = 0
  high = nums.length - 1
  while low <= high
    sum = nums[low] + nums[high]
    if sum == SUM_GOAL
      puts nums[low] * nums[high]
      break
    end
    if sum > SUM_GOAL
      high -= 1
    else
      low += 1
    end
  end
end

def part2
  f, s, t = 0, 1, 2
  nums = read_input.map(&:to_i).sort
  while t < nums.length
    sum = nums[f] + nums[s] + nums[t]
    if sum == SUM_GOAL
      puts nums[f] * nums[s] * nums[t]
      break
    end
    if sum < SUM_GOAL
      t += 1
    else
      if s + 2 == nums.length
        f += 1
        s = f + 1
        t = s + 1
      else
        s += 1
        t = s + 1
      end
    end
  end
end

part2
