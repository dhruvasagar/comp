def alphaValue(word)
  word.scan(/./).inject(0) do |s, w|
    s += w.ord - "A".ord + 1
  end
end

def nameScore(name, index)
  return index * alphaValue(name)
end

data = File.readlines("names.txt", chomp: true)
names = data[0].split(",")
p names.sort.each.with_index.inject(0) { |s, (name, index)|
  name = name[1..-2]
  s += nameScore(name, index+1)
}
