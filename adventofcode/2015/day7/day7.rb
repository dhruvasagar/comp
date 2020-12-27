# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def uint16(num)
  15.downto(0).map { |n| num[n] }.join.to_i(2)
end

class Outputter
  def output
    raise
  end
end

class Sig < Outputter
  def initialize(value)
    @value = value
  end

  def output
    @output ||= uint16(@value)
  end
end

class Wire < Outputter
  attr_accessor :id, :src

  def initialize(id, src=nil)
    @id = id
    @src = src
  end

  def output
    @output ||= @src&.output
  end
end

class Gate < Outputter
  attr_reader :ins

  def initialize(op, ins)
    @op = op
    @ins = ins
  end

  def output
    @output ||= if @op =~ /NOT/
                  uint16(~@ins[0].output)
                elsif @op =~ /OR/
                  uint16(@ins[0].output | @ins[1].output)
                elsif @op =~ /AND/
                  uint16(@ins[0].output & @ins[1].output)
                elsif @op =~ /LSHIFT/
                  uint16(@ins[0].output << @ins[1].output)
                elsif @op =~ /RSHIFT/
                  uint16(@ins[0].output >> @ins[1].output)
                end
  end
end

def propogate_signal(circuit, wire_id, visited)
  wire = circuit[wire_id]
  return wire.output if visited[wire_id]

  visited[wire_id] = true
  src = wire.src
  if src.is_a?(Wire)
    propogate_signal(circuit, src.id, visited)
  elsif src.is_a?(Gate)
    src
      .ins
      .select { |w| w.is_a?(Wire) }
      .each { |w| propogate_signal(circuit, w.id, visited) }
  end
  src.output
end

def part1(circuit, wire_id)
  visited = {}
  propogate_signal(circuit, wire_id, visited)
  circuit[wire_id].output
end

def part2(circuit, wire_id, asignal)
  visited = {}
  circuit['b'].src = Sig.new(asignal)
  propogate_signal(circuit, wire_id, visited)
  circuit[wire_id].output
end

def parse_source(circuit, str)
  ss = str.split
  case ss.size
  when 1
    if str =~ /^\d+/
      Sig.new(str.to_i)
    else
      circuit[str] ||= Wire.new(str)
    end
  when 2
    Gate.new(ss[0], [
      parse_source(circuit, ss[1])
    ])
  when 3
    Gate.new(ss[1], [
      parse_source(circuit, ss[0]),
      parse_source(circuit, ss[2]),
    ])
  end
end

def parse_input(lines)
  lines.each_with_object({}) do |line, circuit|
    src, wire_id = line.split(' -> ')
    wire = parse_source(circuit, wire_id)
    wire.src = parse_source(circuit, src)
    circuit[wire_id] = wire
  end
end

lines = read_input.map(&:chomp)
asignal = part1(parse_input(lines), 'a')
p asignal
p part2(parse_input(lines), 'a', asignal)
