MUL_DIGIT_REGEX = /mul\((\d{1,3}),(\d{1,3})\)/
def score muls
  muls.map {|mul| mul.scan(MUL_DIGIT_REGEX).first.map(&:to_i).reduce(:*)}.reduce(:+)
end

MUL_REGEX = /mul\(\d{1,3},\d{1,3}\)/
def part1 program
  score program.scan(MUL_REGEX)
end

MUL_DODONT_REGEX = /mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)/
def part2 program
  enable = true
  muls = []
  program.scan(MUL_DODONT_REGEX).each do |inst|
    if inst =~ /do\(\)/
      enable = true
    elsif inst =~ /don't\(\)/
      enable = false
    elsif inst =~ /mul/
      muls.push(inst) if enable
    end
  end
  score muls
end

program = readlines.join
p part1 program
p part2 program
