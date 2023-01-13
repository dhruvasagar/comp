package main

import "fmt"

func mySqrt(x int) int {
  l := 0
  r := x + 1

  for l != (r - 1) {
    m := (l + r) / 2

    if m * m <= x {
      l = m
    } else {
      r = m
    }
  }

  return l
}

func main() {
  fmt.Println(mySqrt(4))
  fmt.Println(mySqrt(8))
}
