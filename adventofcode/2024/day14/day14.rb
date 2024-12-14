# frozen_string_literal: true

def parse_input(lines)
  lines.each_with_object({}) {|line, h|
    ps, vs = line.chomp.split
    ppos = ps.split('=').last.split(',').map(&:to_i)
    vel = vs.split('=').last.split(',').map(&:to_i)
    (h[ppos] ||= []) << vel
  }
end

def print_robots(robots)
  rposes = robots.keys
  xmin, xmax = rposes.map(&:first).minmax
  ymin, ymax = rposes.map(&:last).minmax
  ymin.upto(ymax).each {|y|
    xmin.upto(xmax).each {|x|
      pos = [x, y]
      print robots.key?(pos) ? robots[pos].size : '.'
    }
    puts
  }
end

def move_robots(robots,xmax, ymax, time)
  robots.keys.each_with_object({}) {|pos, h|
    x, y = pos
    robots[pos].each {|(vx, vy)|
      npos = [(x + time * vx) % xmax, (y + time * vy) % ymax]
      (h[npos] ||= []) << [vx, vy]
    }
  }
end

def score(robots, xmax, ymax)
  xmid, ymid = xmax/2, ymax/2
  q1, q2, q3, q4 = 1, 1, 1, 1
  q1 = (0...ymid).map {|y|
    (0...xmid).map {|x|
      pos = [x, y]
      robots.key?(pos) ? robots[pos].size : 0
    }.sum
  }.sum
  q2 = (0...ymid).map {|y|
    ((xmid+1)...xmax).map {|x|
      pos = [x, y]
      robots.key?(pos) ? robots[pos].size : 0
    }.sum
  }.sum
  q3 = ((ymid+1)...ymax).map {|y|
    (0...xmid).map {|x|
      pos = [x, y]
      robots.key?(pos) ? robots[pos].size : 0
    }.sum
  }.sum
  q4 = ((ymid+1)...ymax).map {|y|
    ((xmid+1)...xmax).map {|x|
      pos = [x, y]
      robots.key?(pos) ? robots[pos].size : 0
    }.sum
  }.sum
  q1 * q2 * q3 * q4
end

def part1(robots, xmax, ymax, time) =
  score(move_robots(robots, xmax, ymax, time), xmax, ymax)

def part2(robots, xmax, ymax) =
  (1..).each {|time|
    nrobots = move_robots(robots, xmax, ymax, time)
    return time if nrobots.keys.size == nrobots.values.flatten(1).size
  }

def tdiff(s,e) = "#{(e - s) * 1000}ms"

robots = parse_input(ARGF.readlines)
s = Process.clock_gettime(Process::CLOCK_MONOTONIC)
# p part1 robots, 11, 7, 100
p part1 robots, 101, 103, 100
e1 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part2 robots, 101, 103
e2 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
puts "Time taken part1: #{tdiff(s,e1)}, part2: #{tdiff(e1,e2)}, total: #{tdiff(s,e2)}"
