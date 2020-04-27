gets.to_i.times do
  n, k = gets.chomp.split.map(&:to_i)
  puts (0..n).map {|i| i.to_s(k).size}.inject(:+)
end
