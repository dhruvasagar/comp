# frozen_string_literal: true

def inside?(grid, pos)
  pos[0] >= 0 && pos[0] < grid[0].size && pos[1] >= 0 && pos[1] < grid.size
end

def dfs(grid, pos, dirn, vis)
  return if !inside?(grid, pos)

  x, y = pos
  c = grid[y][x]
  return if c == '.' && dirn == vis[pos]

  vis[pos] = dirn
  case c
  when '.'
    next_pos = case dirn
    when 'up'
      [x, y - 1]
    when 'down'
      [x, y + 1]
    when 'left'
      [x - 1, y]
    when 'right'
      [x + 1, y]
    end
    dfs(grid, next_pos, dirn, vis)
  when '|'
    case dirn
    when 'up'
      next_pos = [x, y - 1]
      dfs(grid, next_pos, 'up', vis)
    when 'down'
      next_pos = [x, y + 1]
      dfs(grid, next_pos, 'down', vis)
    when 'left', 'right'
      # split to up & down
      dfs(grid, [x, y - 1], 'up', vis)
      dfs(grid, [x, y + 1], 'down', vis)
    end
  when '-'
    case dirn
    when 'up', 'down'
      # split to left & right
      dfs(grid, [x - 1, y], 'left', vis)
      dfs(grid, [x + 1, y], 'right', vis)
    when 'left'
      dfs(grid, [x - 1, y], 'left', vis)
    when 'right'
      dfs(grid, [x + 1, y], 'right', vis)
    end
  when '/'
    case dirn
    when 'up'
      dfs(grid, [x + 1, y], 'right', vis)
    when 'down'
      dfs(grid, [x - 1, y], 'left', vis)
    when 'left'
      dfs(grid, [x, y + 1], 'down', vis)
    when 'right'
      dfs(grid, [x, y - 1], 'up', vis)
    end
  when '\\'
    next_pos = case dirn
    when 'up'
      dfs(grid, [x - 1, y], 'left', vis)
    when 'down'
      dfs(grid, [x + 1, y], 'right', vis)
    when 'left'
      dfs(grid, [x, y - 1], 'up', vis)
    when 'right'
      dfs(grid, [x, y + 1], 'down', vis)
    end
  end
end

grid = ARGF.readlines.map(&:chomp).map(&:chars)
vis = {}
dfs(grid, [0, 0], 'right', vis)
p vis.size

ymax, xmax = grid.size, grid[0].size
p [
  (0...xmax).map {|x| vis={}; dfs(grid, [x, 0], 'down', vis); vis.size}.max,
  (0...xmax).map {|x| vis={}; dfs(grid, [x, ymax - 1], 'up', vis); vis.size}.max,
  (0...ymax).map {|y| vis={}; dfs(grid, [0, y], 'right', vis); vis.size}.max,
  (0...ymax).map {|y| vis={}; dfs(grid, [xmax - 1, 0], 'left', vis); vis.size}.max,
].max
