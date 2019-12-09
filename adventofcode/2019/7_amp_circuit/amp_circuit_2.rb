$opfunc = {
  1 => :add,
  2 => :mul,
  3 => :inp,
  4 => :out,
  5 => :jmpt,
  6 => :jmpf,
  7 => :lt,
  8 => :eql,
}

class IntCode
  attr_reader :input, :output, :name, :pos, :code, :phase, :thread

  def initialize(code, phase)
    @name = "phase#{phase}"
    @phase = phase
    @input = []
    @output = []

    @pos = 0
    @code = code
  end

  def resume
    thread.run
  end

  def param(off, mode)
    index = pos + off
    mode.nil? || mode == 0 ? code[code[index]] : code[index]
  end

  def add(modes=[0])
    p1 = param(1, modes[0])
    p2 = param(2, modes[1])
    res = p1 + p2
    code[code[pos + 3]] = res
    4
  end

  def mul(modes=[0])
    p1 = param(1, modes[0])
    p2 = param(2, modes[1])
    res = p1 * p2
    code[code[pos + 3]] = res
    4
  end

  def inp(modes=nil)
    Thread.stop unless input.length > 0
    inp = input.pop
    puts "#{@name}: input: #{inp}"
    code[code[pos + 1]] = inp
    2
  end

  def out(modes=nil)
    out = code[code[pos + 1]]
    puts "#{@name}: output: #{out}"
    output.push out
    2
  end

  def jmpt(modes=[0])
    p1 = param(1, modes[0])
    p2 = param(2, modes[1])
    return p2 - pos if p1 > 0
    3
  end

  def jmpf(modes=[0])
    p1 = param(1, modes[0])
    p2 = param(2, modes[1])
    return p2 - pos if p1 == 0
    3
  end

  def lt(modes=[0])
    p1 = param(1, modes[0])
    p2 = param(2, modes[1])
    code[code[pos + 3]] = p1 < p2 ? 1 : 0
    4
  end

  def eql(modes=[0])
    p1 = param(1, modes[0])
    p2 = param(2, modes[1])
    code[code[pos + 3]] = p1 == p2 ? 1 : 0
    4
  end

  def run
    @thread = Thread.new(name) {
      @pos = 0
      while pos < code.size
        modes = []
        opcode = code[pos]
        if opcode > 99
          sopcode = opcode.to_s
          opcode = sopcode[-2..-1].to_i
          modes = sopcode[0...-2].scan(/./).map(&:to_i).reverse
        end
        break if opcode == 99

        shift = send($opfunc[opcode], modes)
        @pos += shift
      end
    }
  end
end

def amp_test_feedback(code, test)
  threads = test.map {|t| p = IntCode.new(code, t); p.run}
  threads.each(&:join)

  inp = 0
  amps.each do |amp|
    puts "amp name: #{amp.name}"
    amp.input.unshift amp.phase
    amp.input.unshift inp
    amp.run
    inp = amp.output.pop
    amp.resume if amp.thread.stop?
  end
  inp
end

def find_max_feedback(code, phase_range)
  phase_range.permutation.map do |phase|
    amp_test_feedback(code, phase)
  end.max
end

# code = [3,8,1001,8,10,8,105,1,0,0,21,42,67,84,97,118,199,280,361,442,99999,3,9,101,4,9,9,102,5,9,9,101,2,9,9,1002,9,2,9,4,9,99,3,9,101,5,9,9,102,5,9,9,1001,9,5,9,102,3,9,9,1001,9,2,9,4,9,99,3,9,1001,9,5,9,1002,9,2,9,1001,9,5,9,4,9,99,3,9,1001,9,5,9,1002,9,3,9,4,9,99,3,9,102,4,9,9,101,4,9,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,99]

code = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

phase_range_2 = (5..9).to_a
# puts find_max(code, phase_range)
amp_test_feedback(code, [9,8,7,6,5])
