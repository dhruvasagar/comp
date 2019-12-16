require 'pp'

data = ARGF.readlines.map {|line| line.scan(/./)}

def dlos_count(data, pos)
  angles = {}
  cx, cy = pos
  return 0 if data[cx][cy] == '.'
  data.each.with_index do |row, x|
    row.each.with_index do |cell, y|
      next if cell == "."
      next if pos == [x, y] # self

      ratio = (x.to_f - cx).abs / (y - cy).abs
      angle = if x <= cx && y <= cy
                Math.atan(ratio) * -1
              elsif x >= cx && y <= cy
                Math.atan(ratio)
              elsif x >= cx && y >= cy
                Math::PI - Math.atan(ratio)
              elsif x <= cx && y >= cy
                Math::PI + Math.atan(ratio)
              end
      angles[angle] = 1
    end
  end
  angles.values.sum
end

dlos_map = data.map.with_index do |row, x|
  row.map.with_index do |cell, y|
    dlos_count(data, [x, y])
  end
end

dlos_max = dlos_map.flatten.max
p dlos_max


dlos_max_pos = []
dlos_map.each_index do |x|
  row = dlos_map[x]
  row.each_index do |y|
    if dlos_map[x][y] == dlos_max
      dlos_max_pos = [x, y]
      break
    end
  end
  break unless dlos_max_pos.empty?
end
p dlos_max_pos
