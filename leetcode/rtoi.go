package main

import "fmt"

func romanToInt(s string) int {
  r := 0
  sz := len(s)
  for i := 0; i < sz; i++ {
    c := s[i]
    if c == 'M' {
      r += 1000
    } else if c == 'C' {
      if i+1 < sz && s[i:i+2] == "CM" {
        r += 900
        i++
      } else if i+1 < sz && s[i:i+2] == "CD" {
        r += 400
        i++
      } else {
        r += 100
      }
    } else if c == 'D' {
      r += 500
    } else if c == 'X' {
      if i+1 < sz && s[i:i+2] == "XC" {
        r += 90
        i++
      } else if i+1 < sz && s[i:i+2] == "XL" {
        r += 40
        i++
      } else {
        r += 10
      }
    } else if c == 'L' {
      r += 50
    } else if c == 'I' {
      if i+1 < sz && s[i:i+2] == "IX" {
        r += 9
        i++
      } else if i+1 < sz && s[i:i+2] == "IV" {
        r += 4
        i++
      } else {
        r += 1
      }
    } else if c == 'V' {
      r += 5
    }
  }
  return r
}

func main() {
  fmt.Println(romanToInt("X"))
  fmt.Println(romanToInt("III"))
  fmt.Println(romanToInt("LVIII"))
  fmt.Println(romanToInt("MCMXCIV"))
}
