package main

import (
  "fmt"
  "strings"
)

func justify(s string, maxWidth int) string {
  sz := len(s)
  ws := strings.Split(s, " ")
  wc := len(ws)
  if wc == 1 {
    return fmt.Sprintf("%-*s", maxWidth, s)
  }

  padding := (maxWidth - sz) / (wc - 1)
  res := strings.Join(ws, strings.Repeat(" ", padding + 1))
  if len(res) < maxWidth {
    c := maxWidth - len(res)
    for i := 0; i < c; i++ {
      ws[i] += " "
    }
  }
  return strings.Join(ws, strings.Repeat(" ", padding + 1))
}

func fullJustify(words []string, maxWidth int) []string {
  sz := len(words)
  res := []string{}
  for i := 0; i < sz; i++ {
    ws := ""
    for j := i; j < sz; j++ {
      if len(ws + words[j]) > maxWidth {
        break
      }
      i = j
      ws += words[j] + " "
    }
    ws = ws[:len(ws)-1]
    if i < sz - 1 {
      res = append(res, justify(ws, maxWidth))
    } else {
      res = append(res, fmt.Sprintf("%-*s", maxWidth, ws))
    }
  }
  return res
}

func debug(words []string) {
  fmt.Println("words: ")
  for _, word := range words {
    fmt.Printf("line: %q, width: %d\n", word, len(word))
  }
  fmt.Println()
}

func main() {
  // debug(fullJustify([]string{"This", "is", "an", "example", "of", "text", "justification."}, 16))
  // debug(fullJustify([]string{"What","must","be","acknowledgment","shall","be"}, 16))
  // debug(fullJustify([]string{"Science","is","what","we","understand","well","enough","to","explain","to","a","computer.","Art","is","everything","else","we","do"}, 20))
  // debug(fullJustify([]string{"The","important","thing","is","not","to","stop","questioning.","Curiosity","has","its","own","reason","for","existing."}, 20))
  debug(fullJustify([]string{"ask","not","what","your","country","can","do","for","you","ask","what","you","can","do","for","your","country"}, 20))
}

