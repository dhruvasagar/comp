require 'pp'

$opfunc = {
  1 => 'add',
  2 => 'mul',
}
MAGIC_OUTPUT=19690720

def add(code, pos)
  code[code[pos + 3]] = code[code[pos + 1]] + code[code[pos + 2]]
end

def mul(code, pos)
  code[code[pos + 3]] = code[code[pos + 1]] * code[code[pos + 2]]
end

def intcode(code)
  pos = 0
  while pos < code.size
    opcode = code[pos]
    break if opcode == 99

    send($opfunc[opcode], code, pos)
    pos += 4
  end
  code
end

def pair_num(i, j)
  100 * i + j
end

def find_pair(code)
  nums = (0..99).to_a
  nums.each do |i|
    nums.each do |j|
      c = code.dup
      c[1] = i
      c[2] = j
      r = intcode(c)[0]
      return pair_num(i, j) if r == MAGIC_OUTPUT
    end
  end
end

code = gets.chomp.split(',').map(&:to_i)

puts find_pair(code)
