package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"sort"
	"strings"
)

func runeValue(w rune) int {
	return int(w - 'A' + 1)
}

func alphaValue(word string) int {
	sum := 0
	for _, w := range word {
		sum += runeValue(w)
	}
	return sum
}

func nameScore(name string, index int) int {
	return index * alphaValue(name)
}

func main() {
	data, err := ioutil.ReadFile("names.txt")
	if err != nil {
		log.Fatal("Unable to read names.txt")
	}
	names := strings.Split(string(data), ",")
	sort.Strings(names)
	sum := 0
	for i, name := range names {
		n := name[1 : len(name)-1]
		fmt.Println(n)
		fmt.Printf("name: %s, index: %d, alphaValue: %d, score: %d\n", n, i+1, alphaValue(n), nameScore(n, i+1))
		sum += nameScore(n, i+1)
	}
	fmt.Println(sum)
}
