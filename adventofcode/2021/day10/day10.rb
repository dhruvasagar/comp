# frozen_string_literal: true

def read_input
  ARGF.readlines
end

PAIRS = { '}' => '{', ']' => '[', ')' => '(', '>' => '<' }.freeze
REV_PAIRS = { '{' => '}', '[' => ']', '(' => ')', '<' => '>' }.freeze
SYNTAX_ERROR_SCORE = { ')' => 3, ']' => 57, '}' => 1197, '>' => 25_137 }.freeze
AUTOCOMPLETE_SCORE = { ')' => 1, ']' => 2, '}' => 3, '>' => 4 }.freeze

def corrupted?(line)
  stack = []
  line.each do |c|
    case c
    when '{', '[', '(', '<'
      stack.push(c)
    when '}', ']', ')', '>'
      return c if stack.last != PAIRS[c]

      stack.pop
    end
  end
  stack
end

lines = read_input.map { |l| l.chomp.chars }
lcs = lines.map(&method(:corrupted?))

p lcs.select { |l| l.size == 1 }.map(&SYNTAX_ERROR_SCORE.method(:[])).sum

def p2_score(bcs)
  bcs.reverse.map(&REV_PAIRS.method(:[])).reduce(0) { |r, c| 5 * r + AUTOCOMPLETE_SCORE[c] }
end

ss = lcs.reject { |l| l.size == 1 }.map(&method(:p2_score)).sort
p ss[ss.size / 2]
