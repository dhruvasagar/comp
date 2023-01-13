package main

import "fmt"

func reverse(x int) int {
  r := 0
  for x > 0 {
    d := x % 10
    r = r * 10 + d
    x /= 10
  }
  return r
}

func isPalindrome(x int) bool {
  if x < 0 {
    return false
  }

  return x == reverse(x)
}

func main() {
  fmt.Println(isPalindrome(121))
  fmt.Println(isPalindrome(-121))
  fmt.Println(isPalindrome(10))
}
