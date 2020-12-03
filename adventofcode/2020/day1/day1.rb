# frozen_string_literal: true

def read_input
  ARGF.readlines
end

SUM_GOAL = 2020

def part1(nums)
  low = 0
  high = nums.length - 1
  while low <= high
    sum = nums[low] + nums[high]
    return nums[low] * nums[high] if sum == SUM_GOAL

    if sum > SUM_GOAL
      high -= 1
    else
      low += 1
    end
  end
end

def part2(nums)
  f, s, t = 0, 1, 2
  while t < nums.length
    sum = nums[f] + nums[s] + nums[t]
    return nums[f] * nums[s] * nums[t] if sum == SUM_GOAL

    if sum < SUM_GOAL
      t += 1
    else
      if s + 2 == nums.length
        f += 1
        s = f + 1
      else
        s += 1
      end
      t = s + 1
    end
  end
end

nums = read_input.map(&:to_i).sort
puts part1(nums)
puts part2(nums)
