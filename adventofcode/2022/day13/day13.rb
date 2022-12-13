# frozen_string_literal: true

def read_input
  ARGF.read
end

def parse_input lines
  lines.split("\n\n").map(&:split).map {|p| p.map(&method(:eval)) }
end

def compare(a, b)
  if a.is_a?(Integer) && b.is_a?(Integer)
    a <=> b
  elsif a.is_a?(Array) && b.is_a?(Array)
    (0...[a.length, b.length].min).each { |i|
      c = compare(a[i], b[i])
      return c unless c == 0
    }
    a.length <=> b.length
  else
    if a.is_a?(Integer)
      compare([a], b)
    else
      compare(a, [b])
    end
  end
end

def part1 packet_pairs
  packet_pairs.map.with_index(1).filter { |p, i| compare(*p) <= 0 }.map(&:last).sum
end

def part2 packet_pairs
  dividers = [ [[2]], [[6]] ]
  sorted = (packet_pairs.flatten(1) + dividers).sort(&method(:compare))
  dividers.map { |x| sorted.find_index(x) + 1 }.inject(:*)
end

packet_pairs = parse_input read_input
p part1 packet_pairs
p part2 packet_pairs
