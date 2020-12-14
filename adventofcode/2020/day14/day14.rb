# frozen_string_literal: true

def read_input
  ARGF.readlines
end

BIT_SIZE = 36

def to_size(string)
  format("%#{BIT_SIZE}s", string)
end

def mask(bit, mask)
  return bit if mask == 'X'

  mask
end

def apply_mask(value, mask)
  bits = to_size(value.to_s(2)).chars.map(&:to_i)
  BIT_SIZE.times.map { |i| mask(bits[i], mask[i]) }.join.to_i(2)
end

def parse_mem_assignment(line)
  m = line.match(/^mem\[(\d+)\] = (\d+)$/)
  [m[1], m[2]].map(&:to_i)
end

def part1(lines)
  mask = to_size('0')
  lines.each_with_object({}) do |line, memories|
    if line =~ /mask =/
      mask = line.split(' = ').last
    else
      index, value = parse_mem_assignment(line)
      memories[index] = apply_mask(value, mask)
    end
  end.values.sum
end

def mask2(bit, mask)
  return mask if mask == 'X'

  bit.to_i | mask.to_i
end

def branch_bit(bits, index)
  sbits = bits[0...index]
  lbits = bits[(index + 1)..-1]
  ["#{sbits}0#{lbits}", "#{sbits}1#{lbits}"]
end

def possible_combinations(bits)
  index = bits.index('X')
  return bits unless index

  branch_bit(bits, index)
    .map { |v| possible_combinations(v) }
    .flatten.map(&:to_i)
end

def apply_mad_mask(index, mask)
  bits = to_size(index.to_s(2)).chars
  BIT_SIZE.times.map { |i| mask2(bits[i], mask[i]) }.join
end

def part2(lines)
  mask = to_size('0')
  lines.each_with_object({}) do |line, memories|
    if line =~ /mask =/
      mask = line.split(' = ').last
    else
      index, value = parse_mem_assignment(line)
      masked_value = apply_mad_mask(index, mask)
      possible_combinations(masked_value).each { |pvi| memories[pvi] = value }
    end
  end.values.sum
end

program = read_input.map(&:chomp)
p part1(program)
p part2(program)
