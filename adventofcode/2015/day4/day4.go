package main

import (
	"bufio"
	"crypto/md5"
	"encoding/hex"
	"fmt"
	"os"
	"strings"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func valid(hash string, size int) bool {
	return strings.HasPrefix(hash, strings.Repeat("0", size))
}

func md5String(text string) string {
	hash := md5.Sum([]byte(text))
	return hex.EncodeToString(hash[:])
}

func findOptimalHash(secret string, size int) int {
	num := 0
	for {
		test := fmt.Sprintf("%s%d", secret, num)
		hash := md5String(test)
		if valid(hash, size) {
			return num
		}
		num++
	}
}

func part1(secret string) int {
	return findOptimalHash(secret, 5)
}

func part2(secret string) int {
	return findOptimalHash(secret, 6)
}

func main() {
	secret := readInput()[0]
	fmt.Println(part1(secret))
	fmt.Println(part2(secret))
}
