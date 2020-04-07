MODULO = 1_000_000_007

def max_profit(prices)
  ps = prices.sort.reverse
  profit = 0
  ps.each.with_index do |p, i|
    pr = p - i
    pr = 0 if pr < 0
    profit += pr
  end
  profit % MODULO
end

t = gets.to_i
t.times do
  n = gets.to_i
  prices = gets.chomp.split.map(&:to_i)
  puts max_profit(prices)
end
