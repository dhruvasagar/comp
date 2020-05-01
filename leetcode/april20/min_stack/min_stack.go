package main

import (
	"fmt"
	"math"
)

type MinStack struct {
	items   []int
	minItem int
}

func Constructor() *MinStack {
	return &MinStack{
		items:   []int{},
		minItem: math.MaxInt64,
	}
}

func (stack *MinStack) Push(item int) {
	stack.items = append(stack.items, item)
	if stack.minItem > item {
		stack.minItem = item
	}
}

func (stack *MinStack) Pop() {
	if len(stack.items) == 0 {
		return
	}
	item := stack.Top()
	stack.items = stack.items[:len(stack.items)-1]
	if len(stack.items) == 0 {
		stack.minItem = math.MaxInt64
		return
	}
	if item == stack.minItem {
		stack.minItem = stack.items[0]
		for _, si := range stack.items {
			if stack.minItem > si {
				stack.minItem = si
			}
		}
	}
}

func (stack MinStack) Top() int {
	return stack.items[len(stack.items)-1]
}

func (stack MinStack) GetMin() int {
	return stack.minItem
}

func main() {
	minStack := Constructor()
	// ["MinStack","push","push","push","top","pop","getMin","pop","getMin","pop","push","top","getMin","push","top","getMin","pop","getMin"]
	// [[],[2147483646],[2147483646],[2147483647],[],[],[],[],[],[],[2147483647],[],[],[-2147483648],[],[],[],[]]
	// [2147483647,null,2147483646,null,2147483646,null,null,2147483647,2147483647,null,-2147483648,-2147483648,null,2147483647]
	minStack.Push(2147483646)
	minStack.Push(2147483646)
	minStack.Push(2147483647)
	fmt.Println("top:", minStack.Top())
	minStack.Pop()
	fmt.Println("min:", minStack.GetMin())
	minStack.Pop()
	fmt.Println("min:", minStack.GetMin())
	minStack.Pop()
	minStack.Push(2147483647)
	fmt.Println("top:", minStack.Top())
	fmt.Println("min:", minStack.GetMin())
	minStack.Push(-2147483648)
	fmt.Println("top:", minStack.Top())
	fmt.Println("min:", minStack.GetMin())
	minStack.Pop()
	fmt.Println("min:", minStack.GetMin())
}
