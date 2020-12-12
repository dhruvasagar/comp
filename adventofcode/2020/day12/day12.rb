# frozen_string_literal: true

def read_input
  ARGF.readlines
end

NORTH   = 'N'
SOUTH   = 'S'
EAST    = 'E'
WEST    = 'W'
LEFT    = 'L'
RIGHT   = 'R'
FORWARD = 'F'
DIRECTIONS = [
  NORTH,
  EAST,
  SOUTH,
  WEST
].freeze

def parse_instructions(instructions)
  instructions.map do |i|
    action, *value = i.chars
    [action, value.join.to_i]
  end
end

def manhattan_distance(xpos, ypos)
  xpos.abs + ypos.abs
end

def rotate(facing, direction, degrees)
  index = DIRECTIONS.index(facing)
  shift = degrees / 90
  if direction == LEFT
    index -= shift
  elsif direction == RIGHT
    index += shift
  end
  DIRECTIONS[index % DIRECTIONS.size]
end

def follow_instructions(instructions)
  x, y = 0, 0
  facing = EAST
  instructions.each do |instruction|
    action, value = instruction
    case action
    when NORTH
      y += value
    when SOUTH
      y -= value
    when EAST
      x += value
    when WEST
      x -= value
    when LEFT
      facing = rotate(facing, action, value)
    when RIGHT
      facing = rotate(facing, action, value)
    when FORWARD
      case facing
      when NORTH
        y += value
      when SOUTH
        y -= value
      when EAST
        x += value
      when WEST
        x -= value
      end
    end
  end
  manhattan_distance(x, y)
end

def part1(instructions)
  follow_instructions(instructions)
end

def rotate_waypoint(wx, wy, direction, degrees)
  case direction
  when RIGHT
    case degrees
    when 90
      [wy, -wx]
    when 180
      [-wx, -wy]
    when 270
      [-wy, wx]
    end
  when LEFT
    case degrees
    when 90
      [-wy, wx]
    when 180
      [-wx, -wy]
    when 270
      [wy, -wx]
    end
  end
end

def follow_instructions2(instructions)
  wx, wy = 10, 1
  x, y = 0, 0
  instructions.each do |instruction|
    action, value = instruction
    case action
    when NORTH
      wy += value
    when SOUTH
      wy -= value
    when EAST
      wx += value
    when WEST
      wx -= value
    when LEFT
      wx, wy = rotate_waypoint(wx, wy, action, value)
    when RIGHT
      wx, wy = rotate_waypoint(wx, wy, action, value)
    when FORWARD
      x += value * wx
      y += value * wy
    end
  end
  manhattan_distance(x, y)
end

def part2(instructions)
  follow_instructions2(instructions)
end

instructions = parse_instructions(read_input)
p part1(instructions)
p part2(instructions)
