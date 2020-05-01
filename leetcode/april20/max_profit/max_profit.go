package main

import (
	"fmt"
)

func maxProfit(prices []int) int {
	s := 0
	b := false
	for i := 0; i < len(prices)-1; i++ {
		p := prices[i]
		np := prices[i+1]
		if b {
			if np < p {
				s += p
				b = false
			}
		} else {
			if np > p {
				b = true
				s -= p
			}
		}
	}
	if b {
		s += prices[len(prices)-1]
	}
	return s
}

func main() {
	stock_prices := [][]int{
		[]int{7, 1, 5, 3, 6, 4},
		[]int{1, 2, 3, 4, 5},
		[]int{7, 6, 4, 3, 1},
		[]int{1, 2},
	}
	for _, prices := range stock_prices {
		fmt.Println(maxProfit(prices))
	}
}
