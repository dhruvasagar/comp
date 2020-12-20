class Tile
  attr_reader :id

  def initialize(id:, data:)
    @id = id
    @data = data
    @_data = data # backup for reset
  end

  def top_edge
    @data[0]
  end

  def bottom_edge
    @data[-1]
  end

  def left_edge
    @data.map { |r| r[0] }
  end

  def right_edge
    @data.map { |r| r[-1] }
  end

  def rotate(count = 1)
    count.times { @data = @data.transpose.map(&:reverse) }
    self
  end

  def flip
    @data = @data.map(&:reverse)
    self
  end

  def reset
    @data = @_data
    self
  end

  def edges
    [
      top_edge,
      right_edge,
      bottom_edge,
      left_edge
    ]
  end

  def edge_map
    {
      left: left_edge,
      top: top_edge,
      right: right_edge,
      bottom: bottom_edge
    }
  end

  def match_edge_count(image)
    edges
      .product(image.edges)
      .select { |e1, e2| e1 == e2 || e1.reverse == e2 }
      .size
  end

  def match_orientation(tile)
    semap = edge_map
    temap = tile.edge_map
    keys = semap.keys
    keypairs = keys.product(keys)
    loop do
      k1, k2 = keypairs.shift
      if semap[k1] == temap[k2]
        return {
          match: [k1, k2],
          rev: false
        }
      elsif semap[k1] == temap[k2].reverse
        return {
          match: [k1, k2],
          rev: true
        }
      end
    end
  end

  def to_s
    @data.map(&:join).join("\n")
  end
end
