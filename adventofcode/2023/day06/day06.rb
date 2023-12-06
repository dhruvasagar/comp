# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def count_wins(time, dist)
  1 + time - (2 * 0.upto(time).find_index {|t| dist < (t * (time - t))})
end

ts, ds = read_input
tline = ts.split(":").last
times = tline.split.map(&:to_i)
dline = ds.split(":").last
dists = dline.split.map(&:to_i)

s = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
# Part 1
p times.each_index.reduce(1) {|w, i| w * count_wins(times[i], dists[i])}

# Part 2
p count_wins(tline.gsub(' ', '').to_i, dline.gsub(' ', '').to_i)
e = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
puts "Time #{(e-s) / 1000000}us"
