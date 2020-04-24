package main

import "fmt"

type Node struct {
	key  int
	val  int
	prev *Node
	next *Node
}

func (n *Node) PrintAll(prefix string) {
	n1 := n
	fmt.Println("Printing all", prefix)
	for n1 != nil {
		fmt.Println(n1)
		n1 = n1.next
	}
}

func (n *Node) String() string {
	return fmt.Sprintf("val: %d, prev: %p, next: %p", n.val, n.prev, n.next)
}

func (n *Node) IsRoot() bool {
	return n.prev == nil
}

func (n *Node) IsTail() bool {
	return n.next == nil
}

func (n *Node) Delete() {
	if n == nil {
		return
	}
	if n.prev != nil {
		if n.next != nil {
			n.prev.next = n.next
		} else {
			n.prev.next = nil
		}
	} else {
		if n.next != nil {
			n.next.prev = nil
		}
	}
	n.prev = nil
	n.next = nil
}

type LRUCache struct {
	cap   int
	root  *Node
	tail  *Node
	cache map[int]*Node
}

func Constructor(capacity int) LRUCache {
	return LRUCache{
		cap:   capacity,
		cache: make(map[int]*Node),
	}
}

func (lru *LRUCache) MoveToFront(node *Node) {
	if node.IsRoot() {
		return
	}
	if node.prev != nil {
		node.prev.next = node.next
	}
	if node.next != nil {
		node.next.prev = node.prev
	}
	if node.IsTail() {
		lru.tail = node.prev
	}
	node.prev = nil
	lru.root.prev = node
	node.next = lru.root
	lru.root = node
}

func (lru *LRUCache) DeleteTail() {
	if lru.tail == nil {
		return
	}
	delete(lru.cache, lru.tail.key)
	prev := lru.tail.prev
	if prev != nil {
		prev.next = nil
	}
	lru.tail = prev
	if prev == nil {
		lru.root = nil
	}
}

func (lru *LRUCache) Get(key int) int {
	node, ok := lru.cache[key]
	if !ok {
		return -1
	}
	lru.MoveToFront(node)
	return node.val
}

func (lru *LRUCache) Put(key int, val int) {
	prevVal := lru.Get(key)
	if prevVal != -1 {
		lru.root.val = val
		return
	}
	if len(lru.cache) == lru.cap {
		lru.DeleteTail()
	}
	node := &Node{key: key, val: val}
	lru.cache[key] = node
	if lru.root == nil {
		lru.root = node
		lru.tail = node
		return
	}
	lru.root.prev = node
	node.next = lru.root
	lru.root = node
}

func main() {
	fmt.Println("Test Case 1")
	lruCache := Constructor(2)
	lruCache.Put(1, 1)
	lruCache.Put(2, 2)
	fmt.Println("get 1:", lruCache.Get(1)) // 1
	lruCache.Put(3, 3)
	fmt.Println("get 2:", lruCache.Get(2)) // -1
	lruCache.Put(4, 4)
	fmt.Println("get 1:", lruCache.Get(1)) // -1
	fmt.Println("get 3:", lruCache.Get(3)) // 3
	fmt.Println("get 4:", lruCache.Get(4)) // 4

	fmt.Println("Test Case 2")
	lruCache = Constructor(1)
	lruCache.Put(2, 1)
	fmt.Println(lruCache.Get(2))
	lruCache.Put(3, 2)
	fmt.Println(lruCache.Get(2))
	fmt.Println(lruCache.Get(3))

	fmt.Println("Test Case 3")
	lruCache = Constructor(3)
	lruCache.Put(1, 1)
	lruCache.Put(2, 2)
	lruCache.Put(3, 3)
	lruCache.Put(4, 4)
	fmt.Println(lruCache.Get(4))
	fmt.Println(lruCache.Get(3))
	fmt.Println(lruCache.Get(2))
	fmt.Println(lruCache.Get(1))
	lruCache.Put(5, 5)
	fmt.Println(lruCache.Get(1))
	fmt.Println(lruCache.Get(2))
	fmt.Println(lruCache.Get(3))
	fmt.Println(lruCache.Get(4))
	fmt.Println(lruCache.Get(5))
}
