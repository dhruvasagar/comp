def predict_s(n, a)
  50*a
end

def choose_c(n, a, s, b)
  (s - (a+b))/2
end

def choose_e(n, a, s, b, c, d)
  s - (a+b+c+d)
end

t = gets.to_i
t.times do
  n = gets.to_i
  a = gets.to_i
  s = predict_s(n, a)
  puts s
  b = gets.to_i
  c = choose_c(n, a, s, b)
  puts c
  d = gets.to_i
  e = choose_e(n, a, s, b, c, d)
  puts e
  r = gets.to_i
  puts a+b+c+d+e == s
  if r == -1
    exit
  end
end
