package main

import (
	"fmt"
)

func applyBackspace(word string) string {
	pos := -1
	for i, c := range word {
		if c == '#' {
			pos = i
			break
		}
	}
	if pos == -1 {
		return word
	}
	if pos == 0 {
		word = word[pos+1:]
	} else {
		word = word[:pos-1] + word[pos+1:]
	}
	return applyBackspace(word)
}

func backspaceCompare(S string, T string) bool {
	return applyBackspace(S) == applyBackspace(T)
}

func main() {
	fmt.Println(backspaceCompare("ab#c", "ad#c"))
	fmt.Println(backspaceCompare("ab##", "c#d#"))
	fmt.Println(backspaceCompare("a##c", "#a#c"))
	fmt.Println(backspaceCompare("a#c", "b"))
}
