# frozen_string_literal: true

require 'digest'

def md5(text)
  Digest::MD5.hexdigest(text)
end

def find_index(word)
  n = 0
  loop do
    hash = md5("#{word}#{n}")
    return n if hash.start_with?('00000')

    n += 1
  end
end

def part1(word)
  8.times.do
end

word = gets.chomp
