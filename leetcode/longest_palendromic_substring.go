package main

import "fmt"

func isPalindrome(s string) bool {
  i := 0
  j := len(s) - 1
  if i == j {
    return true
  }

  for i < j {
    if s[i] != s[j] {
      return false
    }

    i++
    j--
  }
  return true
}

func longestPalindrome(s string) string {
  c := 0
  r := ""
  for i := 0; i < len(s); i++ {
    for j := i + 1; j < len(s); j++ {
      if isPalindrome(s[i:j+1]) {
        if c < len(s[i:j+1]) {
          c = len(s[i:j+1])
          r = s[i:j+1]
        }
      }
    }
  }
  return r
}

func main() {
  fmt.Println(longestPalindrome("babab"))
  fmt.Println(longestPalindrome("babad"))
  fmt.Println(longestPalindrome("bb"))
}
