package main

import (
  "fmt"
)

func abs(x int) int {
  if x < 0 {
    return -x
  }

  return x
}

func reverse(x int) int {
  y := abs(x)
  r := 0
  for y > 0 {
    d := y % 10
    r = r * 10 + d
    y /= 10
  }
  if x < 0 {
    r *= -1
  }
  if r < -2147483648 || r > 2147483647 {
    return 0
  }
  return r
}

func main() {
  fmt.Println(reverse(123))
  fmt.Println(reverse(-123))
  fmt.Println(reverse(120))
  fmt.Println(reverse(1534236469))
}
