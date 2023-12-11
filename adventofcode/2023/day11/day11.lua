local M = {}

local Space = "."
local Galaxy = "#"

function M.is_space(c)
  return c == Space
end

function M.is_galaxy(c)
  return c == Galaxy
end

function M.is_empty_row(row)
  return string.find(row, Galaxy) == nil
end

function M.empty_row_indices(galaxy)
  local indices = {}
  for y, row in ipairs(galaxy) do
    if M.is_empty_row(row) then
      table.insert(indices, y)
    end
  end
  return indices
end

function M.empty_col_indices(galaxy)
  local indices = {}
  for x = 1, #galaxy[1] do
    local col = ""
    for y = 1, #galaxy do
      col = col .. string.sub(galaxy[y], x, x)
    end
    if M.is_empty_row(col) then
      table.insert(indices, x)
    end
  end
  return indices
end

function M.mdist(p1, p2)
  local x1, y1 = unpack(p1)
  local x2, y2 = unpack(p2)
  return math.abs(x1 - x2) + math.abs(y1 - y2)
end

function M.dist(p1, p2, ris, cis, m)
  local x1, y1 = unpack(p1)
  local x2, y2 = unpack(p2)
  if x1 > x2 then
    x1, x2 = x2, x1
  end
  if y1 > y2 then
    y1, y2 = y2, y1
  end
  local ex = 0
  for _, ci in pairs(cis) do
    for x = x1, x2 do
      if x == ci then
        ex = ex + 1
      end
    end
  end
  local ey = 0
  for _, ri in pairs(ris) do
    for y = y1, y2 do
      if y == ri then
        ey = ey + 1
      end
    end
  end
  return M.mdist(p1, p2) + (ex + ey) * (m - 1)
end

function M.sum_digits(gps, ris, cis, m)
  local s = 0
  for i = 1, #gps do
    local p1 = gps[i]
    for j = i + 1, #gps do
      local p2 = gps[j]
      s = s + M.dist(p1, p2, ris, cis, m)
    end
  end
  return s
end

function M.part1()
  local galaxy = vim.fn.getline(0, "$")
  local ris = M.empty_row_indices(galaxy)
  local cis = M.empty_col_indices(galaxy)
  local gps = {}
  for y = 1, #galaxy do
    for x = 1, #galaxy[y] do
      if M.is_galaxy(string.sub(galaxy[y], x, x)) then
        table.insert(gps, { x, y })
      end
    end
  end
  return M.sum_digits(gps, ris, cis, 2)
end

function M.part2()
  local galaxy = vim.fn.getline(0, "$")
  local ris = M.empty_row_indices(galaxy)
  local cis = M.empty_col_indices(galaxy)
  local gps = {}
  for y = 1, #galaxy do
    for x = 1, #galaxy[y] do
      if M.is_galaxy(string.sub(galaxy[y], x, x)) then
        table.insert(gps, { x, y })
      end
    end
  end
  return M.sum_digits(gps, ris, cis, 1000000)
end

return M
