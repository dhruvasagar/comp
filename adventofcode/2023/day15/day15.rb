# frozen_string_literal: true

def tfmt(d) = "#{d / 1000000}ms"
def hash(s) = s.chars.map(&:ord).reduce(0) {|a, c| ((a + c) * 17) % 256}
def part1(ops) = ops.reduce(0) {|a, s| a + hash(s)}

def part2(ops)
  ops.reduce({}) {|map, op|
    if op =~ /(.*)=(.*)/
      label, focal = $1, $2.to_i
      box_index = hash(label)
      map[box_index] ||= []

      lindex = map[box_index].find_index {|(l, _)| l == label}

      if map[box_index].empty? || lindex.nil?
        map[box_index] << [label, focal]
      else
        map[box_index][lindex] = [label, focal]
      end
    elsif op =~ /(.*)-/
      label = $1
      box_index = hash(label)
      if map[box_index]
        lindex = map[box_index].find_index {|(l, _)| l == label}
        map[box_index].delete_at(lindex) if lindex
      end
    end
    map
  }.reduce(0) {|s, (k, v)|
      s + v.each_index.reduce(0) { |t, i| t + ((k + 1) * (i + 1) * v[i][1]) }
  }
end

s = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
ops = ARGF.read.chomp.split(",")
p part1(ops)
p part2(ops)
e = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
puts "Time taken: #{tfmt(e - s)}"
