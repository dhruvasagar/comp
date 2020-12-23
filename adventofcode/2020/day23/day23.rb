# frozen_string_literal: true

PART1_COUNT = 100
PART2_COUNT = 10_000_000

def play(nums, count)
  size = nums.size
  min, max = nums.minmax
  count.times do
    picks = nums[1..3]
    picks.each { |p| nums.delete(p) }
    dst = nums[0] - 1 < min ? max : nums[0] - 1
    loop do
      break unless picks.include?(dst)

      dst = dst - 1 < min ? max : dst - 1
    end
    nums.insert(nums.index(dst) + 1, *picks)
    nums = nums.cycle.take(size + 1).drop(1)
  end
  nums
end

# improvement from original, required for part2
def play2(nums, count)
  size = nums.size
  min, max = nums.minmax
  clist = nums.each_with_object({}).with_index do |(n, h), i|
    h[n] = nums[(i + 1) % size]
  end
  num = nums[0]
  count.times do
    pnum = num
    picks = 3.times.map { pnum = clist[pnum] }
    clist[num] = clist[picks[-1]] # remove picks
    dst = num - 1 < min ? max : num - 1
    loop do
      break unless picks.include?(dst)

      dst = dst - 1 < min ? max : dst - 1
    end
    clist[picks[-1]] = clist[dst]
    clist[dst] = picks[0]
    num = clist[num]
  end
  clist
end

def part1(nums)
  size = nums.size
  clist = play2(nums, PART1_COUNT)
  num = 1
  (size - 1).times.map do
    num = clist[num]
  end.join
end

def part2(nums)
  max = nums.max
  nums += ((max + 1)..1_000_000).to_a
  clist = play2(nums, PART2_COUNT)
  n1 = clist[1]
  n2 = clist[n1]
  n1 * n2
end

nums = gets.chomp.chars.map(&:to_i)
p part1(nums.dup)
p part2(nums.dup)
