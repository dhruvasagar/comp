require 'pp'

def gcd(a, b)
  return a if b.nil? || b == 0
  g = gcd(b, a%b)
end

def cipherKey(primes)
  primes.each.with_index.reduce({}) do |r, (p, i)|
    r[p] = ("A".ord + i).chr
    r
  end
end

def solve(n, l, ciphers)
  primes = {}
  (0...(l-1)).each do |i|
    t = gcd(ciphers[i], ciphers[i+1])
    primes[t] = true
    primes[ciphers[i]/t] = true
    primes[ciphers[i+1]/t] = true
  end
  primes = primes.keys.sort
  ckey = cipherKey(primes)
  r = ""
  (0...l).each do |i|
    c1, c2 = ciphers[i], ciphers[i+1]
    p = primes.find do |pi|
      c1 % pi == 0 && (c2.nil? || c2 % pi == 0)
    end
    r += ckey[c1 / p] if r.chars[-1] != ckey[c1 / p]
    r += ckey[p] if r.chars[-2] != ckey[p]
  end
  r
end

t = gets.to_i
t.times do |i|
  n, l = gets.split.map(&:to_i)
  ciphers = gets.chomp.split.map(&:to_i)
  puts "Case ##{i+1}: #{solve(n, l, ciphers)}"
end
