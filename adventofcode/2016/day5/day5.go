package main

import (
	"crypto/md5"
	"fmt"
	"strconv"
	"strings"
)

func findIndex(word string, idx int) (string, int) {
	for {
		idx++
		text := []byte(fmt.Sprintf("%s%d", word, idx))
		hash := fmt.Sprintf("%x", md5.Sum(text))
		if strings.HasPrefix(hash, "00000") {
			return hash[5:7], idx
		}
	}
}

const passwordLength = 8

func part1(word string) string {
	idx := 0
	pass := ""
	var pchar string
	for i := 0; i < passwordLength; i++ {
		pchar, idx = findIndex(word, idx)
		pass += pchar[:1]
	}
	return pass
}

func replaceAtIndex(in string, r rune, i int) string {
	out := []rune(in)
	out[i] = r
	return string(out)
}

func part2(word string) string {
	idx := 0
	pass := "________"
	pchar := ""
	pmap := make(map[byte]bool)
	for i := 0; i < passwordLength; i++ {
		pchar, idx = findIndex(word, idx)
		if pchar[0] < '0' || pchar[0] > '7' || pmap[pchar[0]] {
			i--
			continue
		}
		pmap[pchar[0]] = true
		pidx, _ := strconv.Atoi(pchar[0:1])
		pass = replaceAtIndex(pass, rune(pchar[1]), pidx)
	}
	return pass
}

func main() {
	word := "uqwqemis"
	fmt.Println(part1(word))
	fmt.Println(part2(word))
}
