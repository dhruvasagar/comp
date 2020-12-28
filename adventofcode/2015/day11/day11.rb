# frozen_string_literal: true

def incpassc(char)
  return 'aa' if char == 'z'

  chars = ('a'..'z').to_a
  index = chars.index(char)
  chars[index + 1]
end

def incpass(pass)
  return 'a' if pass.empty?
  return incpassc(pass) if pass.size == 1

  init = pass[0..-2]
  last = pass[-1]
  return init + incpassc(last) if last != 'z'

  incpass(init) + 'a'
end

def inc?(pass)
  pass.chars.map(&:ord).each_cons(3).any? { |a, b, c| a + 1 == b && b + 1 == c }
end

def dbl_pair?(pass)
  pass.chars.each_cons(2).select { |a, b| a == b }.uniq.size >= 2
end

def confusing?(pass)
  pass =~ /[iol]/
end

def valid?(pass)
  return false unless inc?(pass)
  return false if confusing?(pass)
  return false unless dbl_pair?(pass)

  true
end

def nextpass(pass)
  return incpass(pass) unless confusing?(pass)

  index = pass =~ /[iol]/
  remsize = pass.size - index - 1
  "#{pass[0...index]}#{incpassc(pass[index])}#{'a' * remsize}"
end

def part1(pass)
  loop do
    pass = nextpass(pass)
    break if valid?(pass)
  end
  pass
end

pass = gets.chomp
p pass = part1(pass)
p part1(pass)
