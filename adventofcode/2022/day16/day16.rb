# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def parse_input lines
  lines.reduce({}) do |s, line|
    if /^Valve (..) has flow rate=(\d+); tunnel(s)? lead(s)? to valve(s)? ([A-Z, ]+)$/ =~ line
      vname = $1
      s[vname] = {
        rate: $2.to_i,
        valves: $6.split(", ")
      }
      s
    end
  end
end

class Pipes
  attr_reader :dp, :valves

  def initialize(valves)
    @mem = {}
    @valves = valves

    @dp = Hash.new(1000)
    valves.each do |n, v|
      v[:valves].each do |u|
        @dp[[u,n]] = 1
      end
    end

    # floyd-warshall
    v = valves.keys
    v.product(v).product(v).map(&:flatten).each do |k, i, j|
      @dp[[i,j]] = [@dp[[i,j]], @dp[[i,k]] + @dp[[k,j]]].min
    end
  end

  def search(t, vs, u='AA', e=false)
    @mem[[t, vs, u, e]] ||= (vs.map do |v|
      valves[v][:rate] * (t - dp[[u,v]] - 1) + search(t - dp[[u,v]] - 1, vs-[v], v, e) if dp[[u,v]] < t
    end.compact + [e ? search(26, vs) : 0]).max
  end
end

valves = parse_input read_input
f = valves.reduce([]) { |h, (n, v)| h << n if v[:rate] != 0; h }
pipes = Pipes.new(valves)
p pipes.search(30, f)
p pipes.search(26, f, 'AA', true)
