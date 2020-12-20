class Image
  def initialize(tiles:)
    @tiles = tiles
    @tile_map = tiles.map { |t| [t.id, t] }.to_h
  end

  def corners
    @corners ||= @tiles.select do |tile|
      adj_mat[tile.id][:neighbors].size == 2
    end
  end

  def adj_mat
    @adj_mat ||= @tiles.each_with_object({}) do |tile, h|
      neighbors = (@tiles - [tile]).select do |ntile|
        tile.match_edge_count(ntile).positive?
      end
      h[tile.id] = {
        ncount: neighbors.size,
        neighbors: neighbors.map do |ntile|
          {
            id: ntile.id
          }.merge(tile.match_orientation(ntile))
        end
      }
    end
  end

  def stitch_tiles
    adj_mat
  end
end
