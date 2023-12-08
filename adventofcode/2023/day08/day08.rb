# frozen_string_literal: true

def read_input
  ARGF.readlines
end

Dest = Struct.new(:left, :right)
def parse_map(lines)
  lines.reduce({}) { |map, line|
    source, dest_tuple = line.split("=").map(&:strip)
    dests = dest_tuple.split(",").map(&:strip)
    map[source] = Dest.new dests[0][1..], dests[1][0..-2]
    map
  }
end

def part1(map, steps)
  start = 'AAA'
  steps.chars.cycle.reduce(0) {|s, c|
    break s if start == 'ZZZ'

    start = c == 'L' ? map[start].left : map[start].right
    s + 1
  }
end

def part2(map, steps)
  map
    .keys
    .select {|n| n.end_with?('A')}
    .map {|n|
      start = n
      steps.chars.cycle.reduce(0) {|s, c|
        break s if start.end_with?('Z')

        start = c == 'L' ? map[start].left : map[start].right
        s + 1
      }
    }.reduce(&:lcm)
end

lines = read_input
steps = lines.first.chomp

map = parse_map(lines[2..])
p part1(map, steps)
p part2(map, steps)
