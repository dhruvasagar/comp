# frozen_string_literal: true

require 'parallel'

def read_input
  ARGF.readlines
end

def pure?(pair)
  return false if pair.nil?
  return true if pair.is_a?(Integer)

  pair.is_a?(Array) && pair.size == 2 && pair.all? { |p| p.is_a?(Integer) }
end

def deep_dup(list)
  list.map do |l|
    next l if l.is_a?(Integer)

    deep_dup(l)
  end
end

class Tree
  attr_accessor :val, :left, :right, :parent, :depth

  def initialize(num, parent = nil, depth = 0)
    @depth = depth
    @parent = parent
    if pure?(num)
      @val = num
    else
      @left = Tree.new(num[0], self, depth + 1)
      @right = Tree.new(num[1], self, depth + 1)
    end
  end

  def leaf?
    pure?(val)
  end

  def regular?
    pure?(val) && val.is_a?(Integer)
  end

  def leaf_pair?
    val.is_a?(Array)
  end

  def to_list
    return val if leaf?

    [left.to_list, right.to_list]
  end

  def to_s
    return val.to_s if leaf?

    "[#{left}, #{right}]"
  end

  def inspect
    return "val: #{val}, depth: #{depth}, parent: #{parent}" if leaf?

    [left, right].inspect
  end

  def ==(other)
    val == other.val && left == other.left && right == other.right && depth == other.depth && parent.to_s == other.parent.to_s
  end

  def left_child?
    self == parent&.left
  end

  def right_child
    self == parent&.right
  end

  def each(&block)
    block.call(self) if leaf?

    left&.each(&block)
    right&.each(&block)
  end

  def find(&block)
    return self if block.call(self)

    node = left&.find(&block)
    node ||= right&.find(&block)
    node
  end

  def select(&block)
    res = []
    each do |n|
      res.push(n) if block.call(n)
    end
    res
  end

  def left_neighbor(node)
    prev_node = nil
    tnode = nil
    each do |n|
      prev_node = tnode
      tnode = n

      continue unless n.leaf?

      break if n == node
    end
    prev_node
  end

  def right_neighbor(node)
    prev_node = nil
    tnode = nil
    each do |n|
      prev_node = tnode
      tnode = n

      break if prev_node == node
    end
    tnode
  end

  def explode
    node = find { |n| n.depth == 4 && n.leaf_pair? }
    return false if node.nil?

    left_node = left_neighbor(node)
    right_node = right_neighbor(node)

    unless left_node.nil?
      if left_node.regular?
        left_node.val += node.val[0]
      else
        left_node.val[1] += node.val[0]
      end
    end

    unless right_node.nil?
      if right_node.regular?
        right_node.val += node.val[1]
      else
        right_node.val[0] += node.val[1]
      end
    end

    node.val = 0
    node.left = nil
    node.right = nil
    # puts "after explode node: #{node.inspect}, tree:\t#{self}"
    true
  end

  def split
    node = find do |n|
      break n if n.regular? && n.val >= 10
      break n if n.leaf_pair? && n.val.any? { |v| v >= 10 }
    end
    return false if node.nil?

    onode = node.inspect
    if node.regular?
      half = node.val.to_f / 2
      node.val = [half.floor, half.ceil]
    else
      if node.val[0] >= 10
        half = node.val[0].to_f / 2
        node.left = Tree.new([half.floor, half.ceil], node, node.depth + 1)
        node.right = Tree.new(node.val[1], node, node.depth + 1)
      else
        half = node.val[1].to_f / 2
        node.left = Tree.new(node.val[0], node, node.depth + 1)
        node.right = Tree.new([half.floor, half.ceil], node, node.depth + 1)
      end
      node.val = nil
    end

    # puts "after split node: #{onode}, tree:\t#{self}"
    true
  end

  def reduce
    loop do
      proot = to_s
      explode || split
      break if to_s == proot
    end
    self
  end

  def add(other)
    Tree.new([to_list, other.to_list].map(&method(:deep_dup)))
  end

  def magnitude
    return val if regular?
    return 3 * val[0] + 2 * val[1] if leaf_pair?

    3 * left.magnitude + 2 * right.magnitude
  end
end

def part1(trees)
  r = trees[0].add(trees[1]).reduce
  trees[2..].each do |n|
    r = r.add(n).reduce
  end
  puts r.magnitude
end

def part2(trees)
  max = Parallel.map(trees.permutation(2).to_a) do |t1, t2|
    [
      t1.add(t2).reduce.magnitude,
      t2.add(t1).reduce.magnitude
    ].max
  end.max
  puts max
end

lines = read_input.map { |l| eval l }
trees = lines.map { |l| Tree.new(l) }
part1(trees)
part2(trees)
