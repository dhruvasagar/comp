package main

import (
	"fmt"
)

type TreeNode struct {
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
	tree := &TreeNode{Val: values[0]}
	node := tree
	fmt.Println("full:", IsFull(node))
	for i := 1; i < len(values); i++ {
		cnode := &TreeNode{Val: values[i]}
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

func diameter(t *TreeNode) (int, int) {
	if t == nil {
		return 0, 0
	}

	ldiam, lheight := diameter(t.Left)
	rdiam, rheight := diameter(t.Right)

	diam := max(max(ldiam, rdiam), lheight+rheight)
	return diam, 1 + max(lheight, rheight)
}

func diameterOfBinaryTree(t *TreeNode) int {
	diam, _ := diameter(t)
	return diam
}

func main() {
	tree := NewTree([]int{1, 2, 3, 4, 5})
	fmt.Println(tree)
	fmt.Println(diameterOfBinaryTree(tree))
}
