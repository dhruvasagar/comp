$opfunc = {
  1 => 'add',
  2 => 'mul',
  3 => 'inp',
  4 => 'out',
  5 => 'jmpt',
  6 => 'jmpf',
  7 => 'lt',
  8 => 'eql',
}

$in = []
$out = []

def param(code, pos, mode)
  mode.nil? || mode == 0 ? code[code[pos]] : code[pos]
end

def add(code, pos, modes=[0])
  res = param(code, pos + 1, modes[0]) + param(code, pos + 2, modes[1])
  code[code[pos + 3]] = res
  4
end

def mul(code, pos, modes=[0])
  p1 = param(code, pos + 1, modes[0])
  p2 = param(code, pos + 2, modes[1])
  res = p1 * p2
  code[code[pos + 3]] = res
  4
end

def inp(code, pos, modes=nil)
  input = $in.pop
  puts "input: #{input}"
  code[code[pos + 1]] = input
  2
end

def out(code, pos, modes=nil)
  output = code[code[pos + 1]]
  puts "output: #{output}"
  $out.push output
  2
end

def jmpt(code, pos, modes=[0])
  p1 = param(code, pos + 1, modes[0])
  p2 = param(code, pos + 2, modes[1])
  return p2 - pos if p1 > 0
  3
end

def jmpf(code, pos, modes=[0])
  p1 = param(code, pos + 1, modes[0])
  p2 = param(code, pos + 2, modes[1])
  return p2 - pos if p1 == 0
  3
end

def lt(code, pos, modes=[0])
  p1 = param(code, pos + 1, modes[0])
  p2 = param(code, pos + 2, modes[1])
  code[code[pos + 3]] = p1 < p2 ? 1 : 0
  4
end

def eql(code, pos, modes=[0])
  p1 = param(code, pos + 1, modes[0])
  p2 = param(code, pos + 2, modes[1])
  code[code[pos + 3]] = p1 == p2 ? 1 : 0
  4
end

def intcode(code)
  pos = 0
  while pos < code.size
    modes = []
    opcode = code[pos]
    puts "pos: #{pos}, opcode: #{opcode}"
    if opcode > 99
      sopcode = opcode.to_s
      opcode = sopcode[-2..-1].to_i
      modes = sopcode[0...-2].scan(/./).map(&:to_i).reverse
    end
    break if opcode == 99

    shift = send($opfunc[opcode], code, pos, modes)
    pos += shift
  end
end

def amp_test(code, test)
  inp = 4
  test.each do |t|
    $in.unshift t
    $in.unshift inp
    intcode(code)
    inp = $out.pop
  end
  inp
end

def find_max(code, phase_range)
  phase_range.permutation.map do |phase|
    p phase
    amp_test(code, phase)
  end.max
end

def amp_test_feedback(code, test)
  inp = 0
  test.cycle do |t|
    $in.unshift t
    $in.unshift inp
    intcode(code)
    inp = $out.pop
    puts "input from output: #{inp}"
  end
  inp
end

def find_max_feedback(code, phase_range)
  phase_range.permutation.map do |phase|
    p phase
    amp_test_feedback(code, phase)
  end.max
end

# code = [3,8,1001,8,10,8,105,1,0,0,21,42,67,84,97,118,199,280,361,442,99999,3,9,101,4,9,9,102,5,9,9,101,2,9,9,1002,9,2,9,4,9,99,3,9,101,5,9,9,102,5,9,9,1001,9,5,9,102,3,9,9,1001,9,2,9,4,9,99,3,9,1001,9,5,9,1002,9,2,9,1001,9,5,9,4,9,99,3,9,1001,9,5,9,1002,9,3,9,4,9,99,3,9,102,4,9,9,101,4,9,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99]

code=[3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]

# phase_range = (0..4).to_a
# phase_range_2 = (5..9).to_a
# puts find_max(code, phase_range)
# puts find_max_feedback(code, phase_range_2)

amp_test(code, [0, 1, 2, 3, 4])
