# frozen_string_literal: true

def is_key?(grid) = grid[-1].all? {|c| c == '#'}
def is_lock?(grid) = grid[0].all? {|c| c == '#'}

def key_to_heights(key) = key.transpose.map {|l| l.reverse.rindex('#')}
def lock_to_pin_heights(lock) = lock.transpose.map {|l| l.rindex('#')}

def key_fits_lock(key, lock) = key.zip(lock).map(&:sum).all? {|h| h < 6}

def part1(keys, locks)
  key_heights = keys.map {|k| key_to_heights(k)}
  lock_pin_heights = locks.map {|l| lock_to_pin_heights(l)}

  key_heights.product(lock_pin_heights).filter {|k,l| key_fits_lock(k, l)}.size
end
def part2(keys, locks) = 0
def tdiff(s,e) = "#{(e - s) * 1000}ms"

lines = ARGF.read.split("\n\n").map {|l| l.split("\n").map(&:chars)}
keys, locks = lines.partition {|l| is_key?(l)}
s = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part1 keys, locks
e1 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part2 keys, locks
e2 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
puts "Time taken part1: #{tdiff(s,e1)}, part2: #{tdiff(e1,e2)}, total: #{tdiff(s,e2)}"
