# frozen_string_literal: true

def read_input
  ARGF.readlines
end

HAPPINESS_REGEX = /^(\w+) would (gain|lose) (\d+) happiness .* (\w+)\.$/.freeze

def gain(gol)
  gol == 'gain' ? 1 : -1
end

def parse_line(line, map)
  m = HAPPINESS_REGEX.match(line)
  map[m[1]] ||= {}
  map[m[1]][m[4]] = gain(m[2]) * m[3].to_i
end

def parse_input(lines)
  lines.each_with_object({}) { |line, map| parse_line(line, map) }
end

lines = read_input
dmap = parse_input(lines)

def hsum(arr, map)
  hsum = map[arr[-1]][arr[0]] + map[arr[0]][arr[-1]]
  arr.each_cons(2).reduce(hsum) { |h, (a1, a2)| h + map[a1][a2] + map[a2][a1] }
end

def part1(map)
  ps = map.keys
  cs = ps[1..-1].permutation.map { |ri| [ps[0]] + ri }
  cs.map { |ci| hsum(ci, map) }.max
end

def part2(map)
  map.keys.each { |m| map[m]['You'] = 0 }
  map['You'] = Hash.new(0)
  part1(map)
end

p part1(dmap)
p part2(dmap)
