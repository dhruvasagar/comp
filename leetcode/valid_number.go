package main

import "fmt"

func isDecimal(s string) bool {
  i := 0
  sz := len(s)
  if sz == 0 {
    return false
  }

  if s[i] == '+' || s[i] == '-' {
    i++
    if i == sz {
      return false
    }
  }

  dec := false
  if s[i] == '.' {
    dec = true
    i++
    if i == sz {
      return false
    }
  }

  e := false
  if s[i] == 'e' || s[i] == 'E' {
    return false
  }

  for ;i < sz; i++ {
    if s[i] == '.' {
      if dec {
        return false
      }
      dec = true
      continue
    }

    if s[i] == 'e' || s[i] == 'E' {
      if e {
        return false
      }
      e = true
      if i+1 == sz {
        return false
      }
      return isInteger(s[i+1:])
    }

    if s[i] < '0' || s[i] > '9' {
      return false
    }
  }
  return true
}

func isInteger(s string) bool {
  i := 0
  sz := len(s)
  if sz == 0 {
    return false
  }

  if s[i] == '+' || s[i] == '-' {
    i++
    if i == sz {
      return false
    }
  }

  for ;i < sz; i++ {
    if s[i] < '0' || s[i] > '9' {
      return false
    }
  }
  return true
}

func isNumber(s string) bool {
  return isDecimal(s) || isInteger(s)
}

func main() {
  fmt.Println("Valid Numbers:")
  fmt.Println(isNumber("2"))
  fmt.Println(isNumber("0089"))
  fmt.Println(isNumber("-.9"))
  fmt.Println(isNumber("2e10"))
  fmt.Println(isNumber("-90E3"))
  fmt.Println(isNumber("3e+7"))
  fmt.Println(isNumber("+6e-1"))
  fmt.Println(isNumber("53.5e93"))
  fmt.Println(isNumber("-123.456e789"))
  fmt.Println(isNumber("3."))

  fmt.Println("Invalid Numbers:")
  fmt.Println(isNumber("4e+"))
  fmt.Println(isNumber("+."))
  fmt.Println(isNumber("abc"))
  fmt.Println(isNumber("1a"))
  fmt.Println(isNumber("1e"))
  fmt.Println(isNumber("--6"))
  fmt.Println(isNumber("-+3"))
  fmt.Println(isNumber("95a54e53"))
  fmt.Println(isNumber("99e2.5"))
  fmt.Println(isNumber("e9"))
}
