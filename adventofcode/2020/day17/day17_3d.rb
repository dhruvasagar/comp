# frozen_string_literal: true

ACTIVE = '#'
INACTIVE = '.'
CYCLE_COUNT = 6

def read_input
  ARGF.readlines
end

def display(cubes)
  cubes.group_by(&:z).each do |z, cz|
    puts "z=#{z}"
    puts (cz.group_by(&:y).values.map do |cy|
      cy.sort_by(&:x).map(&:state).join
    end.join("\n"))
    puts
  end
end

$cube_map = {}

def adjacent(cube)
  cubes = []
  (-1..1).each do |i|
    (-1..1).each do |j|
      (-1..1).each do |k|
        next if i.zero? && j.zero? && k.zero?

        xi, yj, zk = cube.x + i, cube.y + j, cube.z + k
        key = cache_key(xi, yj, zk)
        next unless $cube_map.key?(key)

        cubes << $cube_map[key]
      end
    end
  end
  cubes
end

def inside?(cubes, cube)
  min, max = cubes.map(&:x).minmax
  return false if cube.x < min || cube.x > max
  return false if cube.y < min || cube.y > max
  return false if cube.z < min || cube.z > max

  true
end

def expand(cubes)
  min, max = cubes.map(&:x).minmax
  (min-1).upto(max+1).each do |i|
    (min-1).upto(max+1).each do |j|
      (min-1).upto(max+1).each do |k|
        key = cache_key(i, j, k)
        next if $cube_map[key]

        cubes << cache_cube(Cube.new(x: i, y: j, z: k))
      end
    end
  end
end

def simulate(cubes)
  acubes = []
  icubes = []
  cubes.each do |cube|
    alen = adjacent(cube).select(&:active?).size
    if cube.active?
      icubes << cube unless [2, 3].include?(alen)
    elsif alen == 3
      acubes << cube
    end
  end
  acubes.each do |cube|
    cube.state = ACTIVE
  end
  icubes.each do |cube|
    cube.state = INACTIVE
  end
end

def part1(cubes)
  cs = cubes.dup
  CYCLE_COUNT.times do
    expand(cs)
    simulate(cs)
  end
  cs.select(&:active?).size
end

class Cube
  attr_accessor :x, :y, :z, :state

  def initialize(x:, y:, z: 0, state: INACTIVE)
    @x, @y, @z, @state = x, y, z, state
  end

  def active?
    @state == ACTIVE
  end
end

def cache_key(x, y, z)
  "#{x},#{y},#{z}"
end

def cache_cube(cube)
  key = cache_key(cube.x, cube.y, cube.z)
  $cube_map[key] = cube
end

def parse_input(data)
  data.map.with_index do |row, i|
    row.map.with_index do |char, j|
      cache_cube(Cube.new(x: j, y: i, z: 0, state: char))
    end
  end.flatten
end

data = read_input.map(&:chomp).map(&:chars)
p part1(parse_input(data))
