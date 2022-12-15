# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def mdist p1, p2
  x1, y1 = p1
  x2, y2 = p2
  (x1 - x2).abs + (y1 - y2).abs
end

def parse_input lines
  lines.reduce({}) do |s, line|
    if /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/ =~ line
      sp = [$1.to_i, $2.to_i]
      bp = [$3.to_i, $4.to_i]
      s[sp] = {
        bpos: bp,
        range: mdist(sp, bp),
      }
      s
    end
  end
end

def cannot_have_beacon? sensors, point
  return false if sensors.has_key?(point)
  return false if sensors.any? {|_, v| v[:bpos] == point}

  sensors.any? do |p, b|
    mdist(p, point) <= b[:range]
  end
end

def count_pos_without_beacon sensors, xmm, y
  xmin, xmax = xmm
  (xmin..xmax).count { |xi| cannot_have_beacon?(sensors, [xi, y]) }
end

def part1 sensors
  xmm = sensors.flat_map {|p, b| [p[0], p[0] - b[:range], p[0] + b[:range]]}.minmax
  count_pos_without_beacon sensors, xmm, 2000000
end

def tuning_frequency point
  x, y = point
  x * 4000000 + y
end

def part2 sensors
  sensors.each do |sp, b|
    sx, sy = sp
    # search for points range+1 distance away from sensors
    (0..(b[:range]+1)).each do |dx|
      dy = b[:range] + 1 - dx

      [[-1, -1], [-1, 1], [1, -1], [1, 1]].each do |signx, signy|
        x, y = [sx + (dx * signx), sy + (dy * signy)]
        point = [x, y]

        # next unless (0..20).include?(x) && (0..20).include?(y)
        next unless (0..4000000).include?(x) && (0..4000000).include?(y)

        return tuning_frequency(point) if sensors.all? { |p, b| mdist(p, point) > b[:range] }
      end
    end
  end
end

sensors = parse_input read_input
p part1 sensors
p part2 sensors
