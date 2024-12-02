def is_safe(report, min, max)
  unsafe = false
  is_inc = report[0] < report[1]
  0.upto(report.length-2) do |i|
    diff = report[i+1] - report[i]
    return false if diff.abs < min || diff.abs > max || (is_inc && diff < 0) || (!is_inc && diff > 0)
  end
  return true
end

def is_safe_dampened(report, min, max)
  len = report.size
  len.times.each do |i|
    if i == 0
      return true if is_safe(report[1..-1], min, max)
    else
      return true if is_safe(report[0..(i-1)] + report[(i+1)..-1], min, max)
    end
  end
  return false
end

def part1(reports)
  reports.map {|report| is_safe(report, 1, 3)}.count(true)
end

def part2(reports)
  reports.map {|report| is_safe(report, 1, 3) || is_safe_dampened(report, 1, 3)}.count(true)
end

reports = readlines.map {|line| line.split.map(&:to_i)}
p part1 reports
p part2 reports
