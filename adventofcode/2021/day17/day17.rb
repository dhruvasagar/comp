# frozen_string_literal: true

def read_input
  ARGF.read
end

def to_range(s)
  Range.new(*s.split('..').map(&:to_i))
end

def within_target?(target, point)
  x, y = point
  target[0].include?(x) && target[1].include?(y)
end

def maxpos(vel)
  vel = vel.abs
  ( vel * vel + vel ) / 2
end

def posx(vx0, time)
  if vx0.abs > time
    if vx0.negative?
      vx0 * time + time * (time - 1) / 2
    else
      vx0 * time - time * (time - 1) / 2
    end
  else
    nv = maxpos(vx0)
    return -1 * nv if vx0.negative?

    nv
  end
end

def posy(vy0, time)
   vy0 * time - time * (time - 1) / 2
end

# The time when the projectile is back down where y=0
def y0_again(vy0)
  return 0 if vy0.negative?

  2 * vy0 + 1
end

# The value of x coordinate when the projectile is back to y=0
def x_when_y0_again(vx0, vy0)
  return 0 if vy0.negative?

  posx(vx0, y0_again(vy0))
end

def pos(vstart, time)
  [posx(vstart[0], time), posy(vstart[1], time)]
end

def findx(range)
  s = [0, range.first].min
  vx = (s..).find { |i| maxpos(i) >= range.first }
  xs = []
  loop do
    break if vx > range.last

    t = 1
    nx = 0
    loop do
      px = nx
      nx = posx(vx, t)

      if range.include?(nx)
        xs.push(vx)
        break
      end

      break if nx > range.last

      break if nx == px # maxpos value reached

      t += 1
    end

    vx += 1
  end
  xs
end

def findy(range, vx)
  vy = [0, range[1].first].min
  ys = []
  loop do
    t = 1
    ny = 0
    loop do
      py = ny
      ny = posy(vy, t)

      if within_target?(range, [posx(vx, t), ny])
        ys.push(vy)
        break
      end

      break if ny < range[1].first

      t += 1
    end
    break if vy > range[1].first.abs + 1

    vy += 1
  end
  ys
end

target = read_input.chomp.split(': ').last.split(', ').map { |s| s.split('=').last }.map(&method(:to_range))

xs = findx(target[0])
pairs = []
xs.each { |x| findy(target, x).each { |yy| pairs.push([x, yy]) } }

maxy = pairs.map(&:last).max
# part1
p maxpos(maxy)

# part2
p pairs.size
