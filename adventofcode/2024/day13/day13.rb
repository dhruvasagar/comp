# frozen_string_literal: true
require 'matrix'

class Equation
  attr_accessor :a1, :b1, :c1, :a2, :b2, :c2

  def initialize(a1, b1, c1, a2, b2, c2)
    @a1, @b1, @c1, @a2, @b2, @c2 = a1, b1, c1, a2, b2, c2
  end

  def solve
    coeff_matrix = Matrix[[a1, b1], [a2, b2]]
    det = coeff_matrix.det
    return nil if det == 0

    constants = Matrix.column_vector([c1, c2])
    solution = coeff_matrix.inverse * constants
    x, y = solution.to_a.flatten.map(&:to_i)
    return nil if (x != solution[0, 0] || y != solution[1, 0])

    [x, y]
  end
end

def parse_button(button_str)
  button_str.split(":").last.split(",").map {|i| i.split(/\+|=/).last.to_i}
end

class ClawMachine
  attr_accessor :button_a, :button_b, :prize

  def initialize(claw_input)
    @button_a = parse_button(claw_input[0])
    @button_b = parse_button(claw_input[1])
    @prize = parse_button(claw_input[2])
  end

  def to_s
    "#{button_a}, #{button_b}, #{prize}"
  end

  def solve(n=0)
    Equation.new(
      button_a[0],
      button_b[0],
      n + prize[0],
      button_a[1],
      button_b[1],
      n + prize[1]
    ).solve
  end

  def tokens
    soln = solve
    return 0 if soln.nil?

    x, y = soln
    x * 3 + y
  end

  def tokens2
    soln = solve(10000000000000)
    return 0 if soln.nil?

    x, y = soln
    x * 3 + y
  end
end

def part1(claw_machines) = claw_machines.map {|cm| cm.tokens}.sum
def part2(claw_machines) = claw_machines.map {|cm| cm.tokens2}.sum
def tdiff(s,e) = "#{(e - s) * 1000}ms"

claw_inputs = ARGF.read.split("\n\n").map {|c| c.split("\n")}
claw_machines = claw_inputs.map {|c| ClawMachine.new(c)}
# claw_machines.each {|cm| puts cm}
s = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part1 claw_machines
e1 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part2 claw_machines
e2 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
puts "Time taken part1: #{tdiff(s,e1)}, part2: #{tdiff(e1,e2)}, total: #{tdiff(s,e2)}"
