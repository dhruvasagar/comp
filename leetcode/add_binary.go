package main

import (
  "fmt"
  "strings"
)

func addBinary(a string, b string) string {
  sza := len(a)
  szb := len(b)

  size := sza
  if sza > szb {
    size = sza
    b = strings.Repeat("0", sza-szb) + b
  } else if szb > sza {
    size = szb
    c := b
    b = strings.Repeat("0", szb-sza) + a
    a = c
  }

  res := []rune(strings.Repeat("0", size))
  for i := size-1; i >= 0; i-- {
    if a[i] == '0' && b[i] == '0' {
      res[i] = '0'
    } else if (a[i] == '0' && b[i] == '1') || (a[i] == '1' && b[i] == '0') {
      res[i] = '1'
    } else {
      res[i] = '0'
      return addBinary(addBinary(a[:i], b[:i]), "1") + string(res[i:])
    }
  }
  return string(res)
}

func main() {
  fmt.Println(addBinary("0", "1"))
  fmt.Println(addBinary("1", "1"))
  fmt.Println(addBinary("11", "1"))
  fmt.Println(addBinary("10", "11"))
  fmt.Println(addBinary("1010", "1011"))
  fmt.Println(addBinary("1", "111"))
  fmt.Println(addBinary("1111", "1111"))
}
