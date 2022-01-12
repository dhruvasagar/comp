# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def pix2bin(pix)
  pix == '.' ? '0' : '1'
end

def bin2dec(arr)
  arr.map(&method(:pix2bin)).join.to_i(2)
end

def neighs(point)
  x, y = point
  [
    [x - 1, y -1],
    [x, y - 1],
    [x + 1, y - 1],
    [x - 1, y],
    [x, y],
    [x + 1, y],
    [x - 1, y + 1],
    [x, y + 1],
    [x + 1, y + 1],
  ]
end

def get(image, point, default)
  x, y = point
  return default if x < 0 || y < 0

  image[y][x] || default
rescue
  default
end

def enhance(image, algo, count)
  count.times do |t|
    range = (-1..image.size)
    nimage = range.map do |y|
      range.map do |x|
        d = t % 2 == 1 ? algo[0] : '.'
        sq = neighs([x, y]).map do |gx, gy|
          get(image, [gx, gy], d)
        end
        index = bin2dec(sq)
        algo[index]
      end
    end
    # puts "count: #{t + 1}:"
    # system "clear"
    # puts nimage.map(&:join).join("\n")
    # sleep 0.1
    image = nimage
  end
  image
end

lines = read_input
algo = lines[0].chomp.chars
image = lines[2..].map { |l| l.chomp.chars }

# Part 1
output = enhance(image, algo, 2)
# puts output.map(&:join)
puts output.map { |r| r.select { |p| p == '#' }.size }.sum

# Part 2
output = enhance(output, algo, 48)
puts output.map { |r| r.select { |p| p == '#' }.size }.sum
