# frozen_string_literal: true

def read_input
  ARGF.readlines
end

PART1_LIMIT = 2020
PART2_LIMIT = 30_000_000

# ph : history of index for previous to last occurence of number
# lh : history of index for last occurence of number
def next_number(num, index, numbers, ph, lh)
  if index < numbers.size
    num = numbers[index]
    ph[num] = index
    return num
  end

  if lh[num] # second repeat
    nnum = lh[num] - ph[num]
    ph[nnum] ? lh[nnum] = index : ph[nnum] = index
    ph[num] = index - 1
    num = nnum
  else
    num = 0
    lh[num] = index # works because we know 0 has occurred before
  end
  num
end

def part(numbers, limit)
  index = 0
  lh = {}
  ph = {}
  num = numbers.first
  loop do
    num = next_number(num, index, numbers, ph, lh)
    return num if index + 1 == limit

    index += 1
  end
end

def fast_part(numbers, limit)
  lomem_size = 10_000_000
  mem = {}
  lomem = Array.new(lomem_size) { 0 }

  numbers.each.with_index { |n, i| lomem[n] = i + 1 }
  num = numbers[-1]
  turn = numbers.size

  while turn != limit
    m = -1
    ok = false
    if num < lomem_size
      m = lomem[num]
      ok = m != 0
      lomem[num] = turn
    else
      m = mem[num]
      ok = mem.key?(num)
      mem[num] = turn
    end
    num = if ok
            turn - m
          else
            0
          end
    turn += 1
  end
  num
end

start_numbers = read_input[0].split(',').map(&:to_i)
p fast_part(start_numbers, PART1_LIMIT)
p fast_part(start_numbers, PART2_LIMIT)
