# frozen_string_literal: true
class Network
  attr_accessor :network_map
  def initialize(lines)
    @network_map = {}
    lines.each {|line|
      key, val = line.chomp.split("-")
      (@network_map[key]||=[]) << val
      (@network_map[val]||=[]) << key
    }
  end

  def to_s
    "network_map: #{network_map}\nreverse_network_map: #{reverse_network_map}"
  end

  def all_connected?(nodes)
    !nodes.combination(2).any? {|n1, n2| !network_map[n1].include?(n2)}
  end

  def find_tripplets
    network_map.keys.combination(3).filter {|nodes|
      next if nodes.none? {|n| n.start_with?('t')}

      all_connected?(nodes)
    }
  end

  def find_largest_party
    network_map.keys.map {|k|
      n = [k]
      network_map[k].each {|nk|
        n << nk if all_connected?(n + [nk])
      }
      n
    }.sort_by(&:size).last.sort.join(",")
  end
end

def part1(network) = network.find_tripplets.size
def part2(network) = network.find_largest_party
def tdiff(s,e) = "#{(e - s) * 1000}ms"

lines = ARGF.readlines
network = Network.new(lines)
s = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part1 network
e1 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part2 network
e2 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
puts "Time taken part1: #{tdiff(s,e1)}, part2: #{tdiff(e1,e2)}, total: #{tdiff(s,e2)}"
