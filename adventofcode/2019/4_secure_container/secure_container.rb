min, max = gets.chomp.split('-')

def valid_part1?(n)
  return false if n.size != 6
  return false if n.scan(/./).uniq.length == 6 # at least 2 adjacent digits should be same
  return false if n != n.scan(/./).sort.join
  true
end

def adjacent_check?(n)
  n.scan(/./).reduce(Hash.new(0)) do |r, ni|
    r[ni] += 1
    r
  end.values.select {|i| i == 2}.length > 0
end

part1_list = (min..max).select {|n| valid_part1?(n)}
puts part1_list.length

puts part1_list.select {|n| adjacent_check?(n)}.length
