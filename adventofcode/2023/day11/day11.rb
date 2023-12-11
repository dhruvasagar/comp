# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def is_space?(c) = c == '.'
def is_galaxy?(c) = c == '#'
def is_empty_row?(row) = row.all?(&method(:is_space?))
def empty_row_indexes(galaxy) = galaxy.map.with_index {|row, index| index if is_empty_row?(row)}.compact

def print(galaxy)
  puts "galaxy"
  puts galaxy.map {|row| row.join}.join("\n")
end

def mdist(galaxy, p1, p2)
  x1, y1 = p1
  x2, y2 = p2
  (x1 - x2).abs + (y1 - y2).abs
end

def dist(galaxy, p1, p2, ris, cis, multiplier)
  (x1, x2), (y1, y2) = [p1[0], p2[0]].minmax, [p1[1], p2[1]].minmax
  ex, ey = ((x1..x2).to_a & cis).size, ((y1..y2).to_a & ris).size
  mdist(galaxy, p1, p2) + (ex + ey) * (multiplier - 1)
end

def sum_dists(galaxy, gps, ris, cis, multiplier)
  gps.combination(2).reduce(0) {|s, (p1, p2)| s + dist(galaxy, p1, p2, ris, cis, multiplier)}
end

galaxy = read_input.map(&:chomp).map(&:chars)
gps = []
ris = empty_row_indexes(galaxy)
cis = empty_row_indexes(galaxy.transpose)
galaxy.each_index {|y| galaxy[y].each_index {|x| gps << [x, y] if is_galaxy?(galaxy[y][x])}}
p sum_dists(galaxy, gps, ris, cis, 2)
p sum_dists(galaxy, gps, ris, cis, 1000000)
