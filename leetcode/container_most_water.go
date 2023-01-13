package main

import "fmt"

func min(a, b int) int {
  if a < b {
    return a
  }

  return b
}

func area(h1, h2, w int) int {
  return min(h1, h2) * w
}

func maxArea(height []int) int {
  r := 0
  i := 0
  j := len(height) - 1
  for i < j {
    a := area(height[i], height[j], j - i)
    if r < a {
      r = a
    }

    if height[i] < height[j] {
      i++
    } else {
      j--
    }
  }
  return r
}

func main() {
  fmt.Println(maxArea([]int{1,8,6,2,5,4,8,3,7}))
  fmt.Println(maxArea([]int{1,1}))
}
