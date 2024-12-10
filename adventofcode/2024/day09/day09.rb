# frozen_string_literal: true

def convert_disk_map_to_id_block(disk_map)
  # p disk_map
  # p disk_map.each_slice(2).to_a
  disk_map.map.with_index {|id, i|
    i % 2 == 0 ? [i / 2] * id : ['.'] * id
  }.flatten
end

def defrag_id_block(id_block)
  # p id_block
  id_block.filter_map.with_index {|id, idx|
    next id if id != '.'
    last = id_block.pop
    if last == '.'
      loop {
        last = id_block.pop
        last = nil if idx == id_block.size
        break if last != '.'
      }
    end
    last
  }
end

def block_size(id_block, block, sidx, rev=false)
  idx = sidx
  sid = id_block[idx]
  if rev
    idx -= 1 while sid == id_block[idx]
    sidx - idx
  else
    idx += 1 while sid == id_block[idx]
    idx - sidx
  end
end

def find_last_block_by_size(id_block, cidx, lidx, fsize)
  return nil if lidx < cidx

  last_id = id_block[lidx]
  if last_id != '.'
    lsize = block_size(id_block, last_id, lidx, true)
    return [[last_id] * lsize, lidx - lsize + 1] if lsize <= fsize

    find_last_block_by_size(id_block, cidx, lidx - lsize, fsize)
  else
    lidx -= 1 while id_block[lidx] == '.'
    find_last_block_by_size(id_block, cidx, lidx, fsize)
  end
end

def find_free_block_by_size(id_block, bsize)
  idx = 0
  while idx < id_block.size-1
    id = id_block[idx]
    if id != '.'
      idx += 1
    else
      fsize = block_size(id_block, id, idx)
      return idx if fsize >= bsize

      idx += fsize
    end
  end
end

def defrag_id_block_by_file(id_block)
  lidx = id_block.size-1
  vis = {}
  while lidx >= 0
    lid = id_block[lidx]
    if lid == '.'
      fsize = block_size(id_block, lid, lidx, true)
      lidx -= fsize
    else
      lsize = block_size(id_block, lid, lidx, true)
      fidx = find_free_block_by_size(id_block, lsize)
      if fidx && fidx < lidx
        id_block[fidx...(fidx + lsize)] = [lid] * lsize
        id_block[(lidx-lsize+1)..lidx] = ['.'] * lsize
      end
      lidx -= lsize
    end
  end
  id_block
end

def checksum(defrag_id_block)
  defrag_id_block.filter_map.with_index {|id, idx|
    id.to_i * idx
  }.sum
end

def part1(disk_map) =
  checksum(defrag_id_block(convert_disk_map_to_id_block(disk_map)))

def part2(disk_map) =
  checksum(defrag_id_block_by_file(convert_disk_map_to_id_block(disk_map)))

disk_map = ARGF.read.chomp.chars.map(&:to_i)
p part1 disk_map
p part2 disk_map
