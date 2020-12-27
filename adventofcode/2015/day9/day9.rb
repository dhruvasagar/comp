# frozen_string_literal: true

def dist(distance_matrix, sloc, eloc)
  distance_matrix[sloc][eloc]
end

def part1(distance_matrix)
  distance_matrix.keys.permutation.map do |locs|
    locs.each_cons(2).reduce(0) do |d, (sloc, eloc)|
      d + dist(distance_matrix, sloc, eloc)
    end
  end.min
end

def part2(distance_matrix)
  distance_matrix.keys.permutation.map do |locs|
    locs.each_cons(2).reduce(0) do |d, (sloc, eloc)|
      d + dist(distance_matrix, sloc, eloc)
    end
  end.max
end

def read_input
  ARGF.readlines
end

def parse_line(line, dist_mat)
  ls, distance = line.chomp.split(' = ')
  locations = ls.split(' to ')
  dist_mat[locations[0]] ||= {}
  dist_mat[locations[0]][locations[1]] = distance.to_i
  dist_mat[locations[1]] ||= {}
  dist_mat[locations[1]][locations[0]] = distance.to_i
end

def parse_input(lines)
  lines.each_with_object({}) { |line, h| parse_line(line, h) }
end

lines = read_input
p part1(parse_input(lines))
p part2(parse_input(lines))
