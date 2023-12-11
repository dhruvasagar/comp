let s:space = '.'
let s:galaxy = '#'

function! day11#is_space(c)
  return a:c == s:space
endfunction

function! day11#is_galaxy(c)
  return a:c == s:galaxy
endfunction

function! day11#is_empty_row(row)
  for l:c in a:row
    if day11#is_galaxy(l:c) | return v:false | endif
  endfor
  return v:true
endfunction

function! day11#empty_row_indices(galaxy)
  let indices = []
  for y in range(0, len(a:galaxy)-1)
    if day11#is_empty_row(a:galaxy[y]) | call add(indices, y) | endif
  endfor
  return indices
endfunction

function! day11#empty_column_indices(galaxy)
  let indices = []
  for x in range(0, len(a:galaxy[0])-1)
    let col = []
    for y in range(0, len(a:galaxy)-1)
      call add(col, a:galaxy[y][x])
    endfor
    if day11#is_empty_row(col) | call add(indices, x) | endif
  endfor
  return indices
endfunction

function! day11#mdist(p1, p2)
  let [x1, y1] = a:p1
  let [x2, y2] = a:p2
  return abs(x1 - x2) + abs(y1 - y2)
endfunction

function! day11#dist(p1, p2, ris, cis, m)
  let [x1, y1] = a:p1
  let [x2, y2] = a:p2
  if x1 > x2 | let [x1, x2] = [x2, x1] | endif
  if y1 > y2 | let [y1, y2] = [y2, y1] | endif
  let ex = 0
  for ci in a:cis
    for x in range(x1,x2)
      if x == ci | let ex += 1 | endif
    endfor
  endfor
  let ey = 0
  for ri in a:ris
    for x in range(y1,y2)
      if x == ri | let ey += 1 | endif
    endfor
  endfor
  return day11#mdist(a:p1, a:p2) + (ex + ey) * (a:m - 1)
endfunction

function! day11#sum_dists(gps, ris, cis, m)
  let s = 0
  for i in range(0, len(a:gps)-1)
    let p1 = a:gps[i]
    for j in range(i + 1, len(a:gps)-1)
      let p2 = a:gps[j]
      let s += day11#dist(p1, p2, a:ris, a:cis, a:m)
    endfor
  endfor
  return s
endfunction

function! day11#part1()
  let galaxy = getline(0, '$')
  let ris = day11#empty_row_indices(galaxy)
  let cis = day11#empty_column_indices(galaxy)
  let gps = []
  for y in range(0, len(galaxy)-1)
    for x in range(0, len(galaxy[y])-1)
      if day11#is_galaxy(galaxy[y][x]) | call add(gps, [x, y]) | endif
    endfor
  endfor
  return day11#sum_dists(gps, ris, cis, 2)
endfunction

function! day11#part2()
  let galaxy = getline(0, '$')
  let ris = day11#empty_row_indices(galaxy)
  let cis = day11#empty_column_indices(galaxy)
  let gps = []
  for y in range(0, len(galaxy)-1)
    for x in range(0, len(galaxy[y])-1)
      if day11#is_galaxy(galaxy[y][x]) | call add(gps, [x, y]) | endif
    endfor
  endfor
  return day11#sum_dists(gps, ris, cis, 1000000)
endfunction
