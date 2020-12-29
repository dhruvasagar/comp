# frozen_string_literal: true

def read_input
  ARGF.readlines
end

FLYING_REGEX = /^(\w+) .+ (\d+) .+ (\d+) .+ (\d+).*$/.freeze

def parse_line(line, map)
  m = FLYING_REGEX.match(line)
  name = m[1]
  speed, duration, rest = m[2..4].map(&:to_i)
  map[name] = {
    speed: speed,
    duration: duration,
    rest: rest
  }
end

def parse_input(lines)
  lines.each_with_object({}) { |line, map| parse_line(line, map) }
end

def distance(reindeer, duration)
  period = reindeer[:duration] + reindeer[:rest]
  mult = duration / period
  total = mult * reindeer[:duration]
  over = duration - mult * period
  over = reindeer[:duration] if over > reindeer[:duration]
  total += over
  total * reindeer[:speed]
end

def part1(map, duration)
  map.keys.map { |r| distance(map[r], duration) }.max
end

def part2(map, duration)
  (1..duration).map do |s|
    rds = map.values.map { |r| distance(r, s) }
    maxd = rds.max
    rds.map { |r| r / maxd }
  end.transpose.map(&:sum).max
end

map = parse_input(read_input)
p part1(map, 2503)
p part2(map, 2503)
