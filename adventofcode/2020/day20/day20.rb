# frozen_string_literal: true

require 'math'
require './tile'
require './image'

def read_input
  ARGF.read
end

def part1(image)
  image
    .corners
    .map(&:id)
    .reduce(:*)
end

def part2(image)
  image.stitch_tiles
end

def parse_tile(tile)
  tile = tile.split("\n")
  id = tile[0].split(/[\s:]/)[1].to_i
  Tile.new(id: id, data: tile[1..-1].map(&:chars))
end

tiles = read_input.split("\n\n").map { |tile| parse_tile(tile) }
image = Image.new(tiles: tiles)
p part1(image)
p part2(image)
