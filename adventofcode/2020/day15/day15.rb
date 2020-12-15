# frozen_string_literal: true

def read_input
  ARGF.readlines
end

PART1_LIMIT = 2020
PART2_LIMIT = 30_000_000

# h : history for most recent occurence
# hh : history for previous occurence
def next_number(index, numbers, h, hh)
  if index < numbers.size
    num = numbers[index]
    hh[num] = index
    return num
  end

  num = numbers[index - 1]
  if h[num] # second repeat
    nnum = h[num] - hh[num]
    if hh[nnum]
      h[nnum] = index
    else
      hh[nnum] = index
    end
    hh[num] = index - 1
    num = nnum
  else
    num = 0
    h[num] = index # works because we know 0 has occurred before
  end
  numbers << num
  num
end

def part(numbers, limit)
  index = 0
  h = {}
  hh = {}
  loop do
    num = next_number(index, numbers, h, hh)
    return num if index + 1 == limit

    index += 1
  end
end

start_numbers = read_input[0].split(',').map(&:to_i)
p part(start_numbers, PART1_LIMIT)
p part(start_numbers, PART2_LIMIT)
