package main

import (
	"fmt"
	"strings"
)

type Node struct {
	val  int
	next *Node
}

func New(arr []int) *Node {
	head := &Node{val: arr[0]}
	ptr := head
	for i := 1; i < len(arr); i++ {
		node := &Node{val: arr[i]}
		ptr.next = node
		ptr = node
	}
	return head
}

func (n *Node) middle() *Node {
	ptr := n
	mid := n
	cnt := 1
	for ptr.next != nil {
		cnt++
		if cnt%2 == 0 {
			mid = mid.next
		}
		ptr = ptr.next
	}
	return mid
}

func (n *Node) log() {
	ptr := n
	str := []string{}
	for ptr != nil {
		str = append(str, fmt.Sprintf("%d", ptr.val))
		ptr = ptr.next
	}
	fmt.Printf("[%s]\n", strings.Join(str, ","))
}

func main() {
	arr1 := []int{1, 2, 3, 4, 5}
	arr2 := []int{1, 2, 3, 4, 5, 6}
	New(arr1).middle().log()
	New(arr2).middle().log()
}
