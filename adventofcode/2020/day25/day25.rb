# frozen_string_literal: true

MOD = 20_201_227
SUB_NUM = 7

def mod_pow(base, exp, mod)
  return 0 if mod == 1

  r = 1
  base = base % mod
  while exp.positive?
    r = (r * base) % mod if exp.odd?
    exp /= 2
    base = base**2 % mod
  end
  r
end

def read_input
  ARGF.readlines
end

def find_loop_size(num)
  ls = 1
  loop do
    tr = mod_pow(SUB_NUM, ls, MOD)
    return ls if tr == num

    ls += 1
  end
end

def part1(nums)
  cnum, dnum = nums
  ls = find_loop_size(cnum)
  mod_pow(dnum, ls, MOD)
end

public_keys = read_input.map(&:to_i)
p part1(public_keys)
