# frozen_string_literal: true

def parse_input(lines)
  lines.map(&:chomp).map(&:split).map {
    |(d, n, h)| [d, n.to_i, h[2..-3].to_i(16), h[-2].to_i]
  }
end

def area(cs)
  n = cs.size
  cs.each_index.reduce(0) {|s, i|
    j = (i + 1) % n
    s + (cs[i][0] - cs[j][0]) * (cs[i][1] + cs[j][1])
  }.abs / 2
end

def dxdy(part2 = false)
  if part2
    [[1, 0], [0, 1], [-1, 0], [0, -1]]
  else
    { 'R' => [1, 0], 'L' => [-1, 0], 'U' => [0, -1], 'D' => [0, 1] }
  end
end

def solve(input, part2 = false)
  tn = 0
  x, y = 0, 0
  dirs = dxdy(part2)
  coords = input.reduce([]) {|cs, (d, n, dist, dir)|
    d, n = dir, dist if part2
    tn += n
    dx, dy = dirs[d]
    x, y = [x + n * dx, y + n * dy]
    cs + [[x, y]]
  }
  area(coords) + tn / 2 + 1
end

lines = ARGF.readlines
input = parse_input(lines)
p solve(input)
p solve(input, true)
