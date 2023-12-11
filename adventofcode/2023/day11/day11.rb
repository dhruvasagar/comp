# frozen_string_literal: true

def tfmt(d) = "#{d/1000000}ms"
def is_space?(c) = c == '.'
def is_galaxy?(c) = c == '#'
def is_empty_row?(row) = row.all?(&method(:is_space?))
def empty_row_indexes(galaxy) = galaxy.each_index.select {|i| is_empty_row?(galaxy[i])}

def mdist(p1, p2)
  (x1, y1), (x2, y2) = p1, p2
  (x1 - x2).abs + (y1 - y2).abs
end

def dist(galaxy, p1, p2, ris, cis, m)
  (x1, x2), (y1, y2) = [p1[0], p2[0]].minmax, [p1[1], p2[1]].minmax
  ex, ey = ((x1..x2).to_a & cis).size, ((y1..y2).to_a & ris).size
  mdist(p1, p2) + (ex + ey) * (m - 1)
end

def sum_dists(galaxy, gps, ris, cis, m)
  gps.combination(2).reduce(0) {|s, (p1, p2)| s + dist(galaxy, p1, p2, ris, cis, m)}
end

s = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)

galaxy = ARGF.readlines.map(&:chomp).map(&:chars)
ymax, xmax = galaxy.size-1, galaxy[0].size-1
ris, cis = empty_row_indexes(galaxy), empty_row_indexes(galaxy.transpose)
gps = []
(0..ymax).each {|y| (0..xmax).each {|x| gps << [x, y] if is_galaxy?(galaxy[y][x])}}

s1 = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
p sum_dists(galaxy, gps, ris, cis, 2)
e1 = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)

s2 = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
p sum_dists(galaxy, gps, ris, cis, 1000000)
e2 = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)

e = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)

puts "Time taken: part1: #{tfmt(e1-s1)}, part2: #{tfmt(e2-s2)}, total: #{tfmt(e-s)}"
