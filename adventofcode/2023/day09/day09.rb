# frozen_string_literal: true

def diffs(sequence)
  sequence.each_cons(2).map {|a, b| b - a}
end

def predict_next(sequence)
  next_diffs = diffs(sequence)
  return sequence.first if next_diffs.all?(&:zero?)

  sequence[-1] + predict_next(next_diffs)
end

def predict_prev(sequence)
  next_diffs = diffs(sequence)
  return sequence.first if next_diffs.all?(&:zero?)

  sequence[0] - predict_prev(next_diffs)
end

lines = ARGF.readlines.map {|l| l.split.map(&:to_i)}
p lines.map(&method(:predict_next)).sum
p lines.map(&method(:predict_prev)).sum
