def log(i, o)
  puts "Case ##{i}: #{o}"
end

def solve(n)
  a, b = "", ""
  n.chars.each.with_index do |c, i|
    if c == "4"
      a += "1"
      b += "3"
    else
      a += "0" unless a.empty?
      b += c
    end
  end
  return [a, b].join(' ')
end

t = gets.to_i
t.times do |i|
  n = gets.chomp
  log(i+1, solve(n))
end
