def numOfDigits(n)
  return n.to_s.size
end

def fib(n)
  f = Array.new(n) {0}
  f[1]=1
  (2..n).each do |i|
    f[i] = f[i-1] + f[i-2];
  end
  return f[n]
end

def firstWith1000Digits
  (1..).each do |n|
    return n if numOfDigits(fib(n)) == 1000
  end
end

p firstWith1000Digits
