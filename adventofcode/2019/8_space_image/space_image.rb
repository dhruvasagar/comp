data = gets.chomp.scan(/./)
data_by_layer = data.each_slice(25 * 6).to_a

# Part 1
data_by_layer_zeros_count = data_by_layer.map do |layer|
  layer.inject(0) {|s, l| s += 1 if l == "0"; s}
end
data_by_layer_with_min_zeroes = data_by_layer[data_by_layer_zeros_count.index(data_by_layer_zeros_count.min)]

number_of_ones = data_by_layer_with_min_zeroes.inject(0) {|s, l| s += 1 if l == "1"; s}
number_of_twos = data_by_layer_with_min_zeroes.inject(0) {|s, l| s += 1 if l == "2"; s}

# puts number_of_ones * number_of_twos

# Part 2
def color_of_stack(stack)
  return stack.detect {|s| s != "2"}
end

# p data_by_layer
decoded_image = data_by_layer.transpose.map {|d| color_of_stack(d)}

def draw(image)
  rows = image.each_slice(25).to_a
  rows.each do |row|
    puts row.map {|r| r == "0" ? '_' : '█'}.join.gsub('_', ' ')
  end
end

draw(decoded_image)
