package main

import (
	"fmt"
)

type TreeNode struct {
	ID    int
	Val   int
	Left  *TreeNode
	Right *TreeNode
}

func IsFull(t *TreeNode) bool {
	return t.Left != nil && t.Right != nil
}

func (t *TreeNode) String() string {
	s := ""
	s += fmt.Sprintf("%d\n", t.Val)
	if t.Left != nil {
		s += fmt.Sprintf("%d->%s", t.Val, t.Left)
	}
	if t.Right != nil {
		s += fmt.Sprintf("%d->%s", t.Val, t.Right)
	}
	return s
}

func NewTree(values []int) *TreeNode {
	tree := &TreeNode{ID: 1, Val: values[0]}
	node := tree
	fmt.Println("full:", IsFull(node))
	for i := 1; i < len(values); i++ {
		cnode := &TreeNode{ID: i + 1, Val: values[i]}
		for IsFull(node) {
			if node.Left != nil {
				node = node.Left
			} else {
				node = node.Right
			}
		}
		if node.Left == nil {
			node.Left = cnode
		} else {
			node.Right = cnode
		}
	}
	return tree
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func height(t *TreeNode) int {
	if t == nil {
		return 0
	}

	return 1 + max(height(t.Left), height(t.Right))
}

func diameter(t *TreeNode) int {
	if t == nil {
		return 0
	}

	lheight := height(t.Left)
	rheight := height(t.Right)

	ldiameter := diameter(t.Left)
	rdiameter := diameter(t.Right)

	return max(lheight+rheight, max(ldiameter, rdiameter))
}

func main() {
	tree := NewTree([]int{1, 2, 3, 4, 5})
	fmt.Println(tree)
	fmt.Println(diameter(tree))
}
