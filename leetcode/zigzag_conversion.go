package main

import (
  "fmt"
  "strings"
)

func convert(s string, numRows int) string {
  if numRows == 1 {
    return s
  }

  rs := make([]string, numRows)
  d := 1
  idx := 0
  for _, c := range s {
    rs[idx] += string(c)
    if idx == 0 {
      d = 1
    } else if idx + 1 == numRows {
      d = -1
    }
    idx += d
  }
  return strings.Join(rs, "")
}

func main() {
  fmt.Println(convert("PAYPALISHIRING", 3))
  fmt.Println(convert("PAYPALISHIRING", 4))
  fmt.Println(convert("A", 1))
  fmt.Println(convert("AB", 1))
}
