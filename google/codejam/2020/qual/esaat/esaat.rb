def compliment(bits)
  bits.map {|b| 1-b}
end

def reverse(bits)
  bits.reverse
end

[
  "0001101111".scan(/./).map(&:to_i)
].each do |test|
  p shift_right(test) == compliment(reverse(test))
end
