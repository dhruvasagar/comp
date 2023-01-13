package main

import (
  "fmt"
  "strings"
)

func groups(p string) []string {
  gs := []string{}
  for i, pc := range p {
    if pc == '.' {
      gs = append(gs, ".")
    } else if pc == '*' {
      gs = gs[:len(gs)-1]
      gs = append(gs, fmt.Sprintf("%c%c", p[i-1], p[i]))
    } else {
      gs = append(gs, string(pc))
    }
  }

  return gs
}

func isMatch(s string, p string) bool {
  pgs := groups(p)
  idx := 0
  size := len(s)
  for pi, pg := range pgs {
    if pg == "." {
      if idx == size {
        return false
      }
      idx += 1
    } else {
      if len(pg) == 1 {
        if idx == size {
          return false
        }
        if s[idx] != pg[0] {
          return false
        }
        idx++
      } else {
        for idx < len(s) {
          if pg[0] != '.' && s[idx] != pg[0] {
            break
          }
          if isMatch(s[idx:], strings.Join(pgs[pi + 1:], "")) {
            break
          }
          idx++
        }
      }
    }
  }
  return idx == len(s)
}

func main() {
  fmt.Println(isMatch("aa", "a"))
  fmt.Println(isMatch("aa", "a*"))
  fmt.Println(isMatch("aa", ".*"))
  fmt.Println(isMatch("aa", "a*b*"))
  fmt.Println(isMatch("aa", "a*.b*"))
  fmt.Println(isMatch("aa", "abc"))
  fmt.Println(isMatch("aa", ".*c"))
  fmt.Println(isMatch("aa", ".*.*"))
  fmt.Println(isMatch("aa", "a*a"))
  fmt.Println(isMatch("aa", "aaa"))
  fmt.Println(isMatch("aa", "a*a*"))
  fmt.Println(isMatch("aaa", "ab*a*c*a*"))
  fmt.Println(isMatch("aaa", "ab*a*c*a"))
}
