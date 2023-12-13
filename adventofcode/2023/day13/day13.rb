# frozen_string_literal: true

def tfmt(d) = "#{d/1000000}ms"
def diff(a, b) = (0...a.size).reduce(0) {|s, i| s + (a[i] == b[i] ? 0 : 1)}

def mirror(pattern, d = 0)
  n = pattern.size - 1
  (1..n).map {|i|
    j = [i, n - i + 1].min
    return i if (0...j).reduce(0) {|s, ji| s + diff(pattern[i - ji - 1], pattern[i + ji])} == d
  }
  0
end

s = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
patterns = ARGF.read.split("\n\n").map(&:split).map {|p| p.map(&:chars)}
p patterns.reduce(0) {|s, p| s + 100 * mirror(p) + mirror(p.transpose)}
p patterns.reduce(0) {|s, p| s + 100 * mirror(p, 1) + mirror(p.transpose, 1)}
e = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
puts "Total time: #{tfmt(e-s)}"
