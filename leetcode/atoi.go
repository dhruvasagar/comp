package main

import "fmt"

func myAtoi(s string) int {
  res := 0
  sign := 1
  leading := true
  for _, c := range s {
    if leading && c == 32 { // whitespace
      continue
    }
    if leading && c == 43 { // + sign
      leading = false
      continue
    }
    if leading && c == 45 { // - sign
      leading = false
      sign = -1
      continue
    }
    if c < 48 || c > 57 {
      break
    }

    if c >= 48 && c <= 57 {
      leading = false
      res = res * 10 + (int(c) - 48)
      v := res * sign
      if v < -2147483648 {
        return -2147483648
      } else if v > 2147483647 {
        return 2147483647
      }
    }
  }
  return res * sign
}

func main() {
  fmt.Println(myAtoi("42"))
  fmt.Println(myAtoi("    -42"))
  fmt.Println(myAtoi("4193 with words"))
  fmt.Println(myAtoi("words and 987"))
  fmt.Println(myAtoi("-91283472332"))
  fmt.Println(myAtoi("3.14159"))
  fmt.Println(myAtoi("9223372036854775808"))
}
