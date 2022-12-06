function! aoc#NewMove(count, from, to) abort
  let move = {}
  let move.count = str2nr(a:count)
  let move.from = str2nr(a:from)
  let move.to = str2nr(a:to)
  return move
endfunction

function! aoc#ParseMove(line) abort
  let matches = matchlist(a:line, 'move \(\d\+\) from \(\d\+\) to \(\d\+\)')
  return aoc#NewMove(matches[1], matches[2], matches[3])
endfunction

function! aoc#FindStackTop(index) abort
  let empty_line = search('^$', 'n') " empty line
  let line = empty_line - 1
  let index_line = getline(line)
  let index_col = match(index_line, a:index) + 1 " 0 based index
  while line >= 0
    let crate = getline(line - 1)[index_col]
    if empty(trim(crate)) | break | endif
    let line -= 1
  endwhile
  return [line, index_col]
endfunction

function! aoc#GetColExpr(col)
  return '\%'.(a:col-1).'c.\%'.a:col.'c.\%'.(a:col+1).'c.'
endfunction

function! aoc#AnimateMove(move) abort
  let cnt = a:move.count
  while cnt > 0
    let spos = aoc#FindStackTop(a:move.from)
    let epos = aoc#FindStackTop(a:move.to)
    if epos[0] > 0 | let epos[0] -= 1 | endif
    " echom 'moving ' . a:move.count . ' from ' . a:move.from . ' to ' . a:move.to

    let pos = spos
    let dirn = 'up'
    while pos !=# epos
      let crate = getline(pos[0])[pos[1]-2:pos[1]]

      " delete crate from current pos
      exec ':'.pos[0].'s/'.aoc#GetColExpr(pos[1]).'/   /'

      if dirn ==# 'up'
        if pos[0] > 1
          let pos[0] -= 1
        else
          " create a new empty line for the crate to move
          call append(0, repeat(' ', len(getline(pos[0]))))
          let epos[0] += 1
          if pos[1] < epos[1]
            let dirn = 'right'
          else
            let dirn = 'left'
          endif
        endif
      elseif dirn ==# 'down'
        if pos[0] ==# 1 && empty(trim(getline(pos[0])))
          exec ':' . pos[0] . 'delete'
          let epos[0] -= 1
        endif
        if pos[0] < epos[0]
          let pos[0] += 1
        endif
      elseif dirn ==# 'right'
        if pos[1] < epos[1]
          let pos[1] += 4
        else
          let dirn = 'down'
          exec ':' . pos[0] . 'delete'
          let epos[0] -= 1
        endif
      elseif dirn ==# 'left'
        if pos[1] > epos[1]
          let pos[1] -= 4
        else
          let dirn = 'down'
          exec ':' . pos[0] . 'delete'
          let epos[0] -= 1
        endif
      endif

      " move crate to new pos
      exec ':'.pos[0].'s/'.aoc#GetColExpr(pos[1]).'/'.crate.'/'

      sleep 1m
      redraw!
    endwhile
    let cnt -= 1
  endwhile
endfunction

function! aoc#Part1() abort
  let first_move_line = search('^move', 'n')
  set nolist nohlsearch
  for line in getline(first_move_line, '$')
    let move = aoc#ParseMove(line)
    call aoc#AnimateMove(move)
  endfor
  set hlsearch list
endfunction
