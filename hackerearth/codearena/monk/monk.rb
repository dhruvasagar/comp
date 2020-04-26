n, k = gets.chomp.split(' ').map(&:to_i)
ns = gets.chomp.split(' ').map(&:to_i)

puts ns.sort_by {|n| n % k}.join(' ')
