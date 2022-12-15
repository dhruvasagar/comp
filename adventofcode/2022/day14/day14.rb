# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def parse_input lines
  lines.map {|l| l.split(" -> ").map {|p| p.split(",").map(&:to_i)}}
end

def full_path line
  line.each_cons(2).reduce({}) do |s, (p1, p2)|
    x1, y1 = p1
    x2, y2 = p2

    if x1 == x2 # vertical line
      if y1 < y2
        (y1..y2).each {|yi| s[[x1, yi]] = '#'}
      else
        (y2..y1).each {|yi| s[[x1, yi]] = '#'}
      end
    else        # horizontal line
      if x1 < x2
        (x1..x2).each {|xi| s[[xi, y1]] = '#'}
      else
        (x2..x1).each {|xi| s[[xi, y1]] = '#'}
      end
    end
    s
  end
end

def all_paths path_lines
  path_lines.reduce({}) do |s, ph|
    s.merge(ph)
  end
end

def falling_forever? paths, point
  ymax = paths.keys.map(&:last).max
  _, y = point
  return true if y > ymax
  false
end

def down p
  x, y = p
  [x, y + 1]
end

def left p
  x, y = p
  [x - 1, y]
end

def right p
  x, y = p
  [x + 1, y]
end

def visualize_paths paths, update
  xmin, xmax = paths.keys.map(&:first).minmax
  ymin, ymax = paths.keys.map(&:last).minmax
  ymin = [0, ymin].min
  s = ""
  (ymin..ymax).each do |y|
    (xmin..xmax).each do |x|
      if [x, y] == [500, 0]
        s += '+'
      else
        s += paths[[x, y]] || '.'
      end
    end
    s += "\n"
  end
  puts s
  if update
    sleep (0.01)
    system 'clear'
  end
end

def is_not_wall? paths, point
  paths[point] != '#' && paths[point] != 'o'
end

def simulate_sand paths
  sp = [500, 0]
  highest_resting_sand = [500, 0]
  while true
    return if falling_forever?(paths, sp)

    np = down sp
    sp = if is_not_wall? paths, np
           np
         else
           if is_not_wall? paths, left(np)
             left np
           elsif is_not_wall? paths, right(np)
             right np
           else
             paths[sp] = 'o' # mark as wall
             if highest_resting_sand[1] > sp[1]
               highest_resting_sand[1] = sp[1]
             end
             [500, highest_resting_sand[1] - 1]
           end
         end
    # visualize_paths paths, true
  end
end

def part1 paths
  simulate_sand paths
  paths.count {|_, v| v == 'o'}
end

def visualize_paths_with_floor paths, update
  xmin, xmax = paths.keys.map(&:first).minmax
  ymin, ymax = paths.keys.map(&:last).minmax
  floory = ymax + 2
  ymin = [0, ymin].min
  s = ""
  (ymin..ymax).each do |y|
    (xmin..xmax).each do |x|
      if [x, y] == [500, 0]
        s += '+'
      elsif y == floory
        s += '#'
      else
        s += paths[[x, y]] || '.'
      end
    end
    s += "\n"
  end
  puts s
  if update
    sleep (0.01)
    system 'clear'
  end
end

def is_not_wall_or_floor? paths, floory, point
  return false if point[1] == floory
  paths[point] != '#' && paths[point] != 'o'
end

def simulate_sand_with_floor paths
  sp = [500, 0]
  highest_resting_sand = [500, 0]
  floory = paths.keys.map(&:last).max + 2
  while true
    # return cnt if falling_forever?(paths, sp)
    return if paths[[500, 0]] == 'o'

    np = down sp
    sp = if is_not_wall_or_floor? paths, floory, np
           np
         else
           if is_not_wall_or_floor? paths, floory, left(np)
             left np
           elsif is_not_wall_or_floor? paths, floory, right(np)
             right np
           else
             paths[sp] = 'o' # mark as wall
             if highest_resting_sand[1] > sp[1]
               highest_resting_sand[1] = sp[1]
             end
             [500, highest_resting_sand[1] - 1]
           end
         end
  end
end


def part2 paths
  simulate_sand_with_floor paths
  paths.count {|_, v| v == 'o'}
end

path_points = parse_input read_input
paths = all_paths path_points.map(&method(:full_path))
p part1 paths
p part2 paths
