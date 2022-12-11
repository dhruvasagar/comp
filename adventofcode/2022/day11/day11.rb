# frozen_string_literal: true

def read_input
  ARGF.read
end

class Monkey
  attr_reader :total_inspected, :test
  attr_accessor :index, :items, :operation, :test_target

  def initialize(lines)
    @index = lines[0].split(":").first.split.last.to_i
    @items = lines[1].split(":").last.split(",").map(&:to_i)

    @op = lines[2].split(' = ').last
    @operation = Proc.new {|n, base|
      nn = if /old \+ (\d+)/ =~ @op
        n + $1.to_i
      elsif /old \* (\d+)/ =~ @op
        n * $1.to_i
      else
        n * n
      end
      nn %= base if base
      nn
    }

    @test = lines[3].split('divisible by').last.to_i
    @tidx = lines[4].split.last.to_i
    @fidx = lines[5].split.last.to_i
    @test_target = Proc.new {|n| n % @test == 0 ? @tidx : @fidx }

    @total_inspected = 0
  end

  def play(base)
    (0...items.size).map do |idx|
      item = items.shift
      @total_inspected += 1
      nitem = operation.call(item, base) / 3
      [nitem, test_target.call(nitem)]
    end
  end

  def play2(base)
    (0...items.size).map do |idx|
      item = items.shift
      @total_inspected += 1
      nitem = operation.call(item, base)
      [nitem, test_target.call(nitem)]
    end
  end

  def push(item)
    @items.push(item)
  end

  def to_s
    "Monkey #{index}: items: #{items&.join(", ")}"
  end
  alias_method :inspect, :to_s
end

def round(monkeys)
  base = monkeys.map(&:test).inject(:*)
  monkeys.each do |monkey|
    monkey.play(base).each do |nitem, tidx|
      monkeys[tidx].push(nitem)
    end
  end
end

def round2(monkeys)
  base = monkeys.map(&:test).inject(:*)
  monkeys.each do |monkey|
    monkey.play2(base).each do |nitem, tidx|
      monkeys[tidx].push(nitem)
    end
  end
end

def parse_monkey(monkey_lines)
  Monkey.new(monkey_lines)
end

def parse_input(input)
  input.split("\n\n").map {|ml| parse_monkey(ml.split("\n")) }
end

def part1(monkeys)
  20.times do
    round(monkeys)
  end
  monkeys.map(&:total_inspected).sort.reverse.take(2).inject(:*)
end

def part2(monkeys)
  10000.times do |i|
    round2(monkeys)
  end
  monkeys.map(&:total_inspected).sort.reverse.take(2).inject(:*)
end

lines = read_input
p part1 parse_input(lines)
p part2 parse_input(lines)
