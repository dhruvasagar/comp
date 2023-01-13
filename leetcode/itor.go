package main

import (
  "fmt"
  "strings"
)

func intToRoman(num int) string {
  if num == 1 {
    return "I"
  } else if num > 1 && num < 4 {
    return strings.Repeat("I", num)
  } else if num == 4 {
    return "IV"
  } else if num == 5 {
    return "V"
  } else if num > 5 && num < 9 {
    return fmt.Sprintf("V%s", intToRoman(num - 5))
  } else if num == 9 {
    return "IX"
  } else if num == 10 {
    return "X"
  } else if num > 10 && num < 40 {
    return fmt.Sprintf("X%s", intToRoman(num - 10))
  } else if num == 40 {
    return "XL"
  } else if num > 40 && num < 50 {
    return fmt.Sprintf("XL%s", intToRoman(num - 40))
  } else if num == 50 {
    return "L"
  } else if num > 50 && num < 90 {
    return fmt.Sprintf("L%s", intToRoman(num - 50))
  } else if num == 90 {
    return "XC"
  } else if num > 90 && num < 100 {
    return fmt.Sprintf("XC%s", intToRoman(num - 90))
  } else if num == 100 {
    return "C"
  } else if num > 100 && num < 400 {
    return fmt.Sprintf("C%s", intToRoman(num - 100))
  } else if num == 400 {
    return "CD"
  } else if num > 400 && num < 500 {
    return fmt.Sprintf("CD%s", intToRoman(num - 400))
  } else if num == 500 {
    return "D"
  } else if num > 500 && num < 900 {
    return fmt.Sprintf("D%s", intToRoman(num - 500))
  } else if num == 900 {
    return "CM"
  } else if num > 900 && num < 1000 {
    return fmt.Sprintf("CM%s", intToRoman(num - 900))
  } else if num == 1000 {
    return "M"
  } else {
    return fmt.Sprintf("M%s", intToRoman(num - 1000))
  }
}

func main() {
  fmt.Println(intToRoman(3))
  fmt.Println(intToRoman(58))
  fmt.Println(intToRoman(1994))
}
