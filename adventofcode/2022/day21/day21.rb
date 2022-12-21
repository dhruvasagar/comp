# frozen_string_literal: true

def read_input
  ARGF.readlines.map(&:chomp)
end

def parse_input lines
  lines.reduce({}) do |h, line|
    key, val = line.split(": ")
    vals = val.split
    vals = vals[0].to_i if vals.size == 1
    h[key] = vals
    h
  end
end

def calc vals, root, humn=-1
  return humn if root == 'humn' && humn >= 0
  return vals[root] if vals[root].is_a?(Integer)

  val = vals[root]
  ops = [val[0], val[2]].map {|v| calc(vals, v, humn)}

  return case val[1]
         when '+'
           ops[0] + ops[1]
         when '-'
           ops[0] - ops[1]
         when '*'
           ops[0] * ops[1]
         when '/'
           ops[0] / ops[1]
         end
end

def part1 vals
  calc vals, 'root'
end

def part2 vals
  v1, _, v2 = vals['root']

  low = 0
  high = 10**15
  while low < high
    mid = (low + high) / 2
    diff = calc(vals, v1, mid) - calc(vals, v2, mid)
    if diff == 0
      return mid
    elsif diff > 0
      low = mid
    else
      high = mid
    end
  end
end

lines = read_input
vals = parse_input lines
p part1 vals
p part2 vals
