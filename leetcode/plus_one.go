package main

import "fmt"

func plusOne(digits []int) []int {
  sz := len(digits)
  if digits[sz-1] < 9 {
    digits[sz-1]++
    return digits
  } else {
    if sz == 1 {
      return []int{1, 0}
    }

    return append(plusOne(digits[:sz-1]), 0)
  }
}

func main() {
  fmt.Println(plusOne([]int{1,2,3}))
  fmt.Println(plusOne([]int{9}))
  fmt.Println(plusOne([]int{9, 9, 9, 9}))
}
