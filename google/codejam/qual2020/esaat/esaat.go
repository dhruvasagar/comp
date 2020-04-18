package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func query(b int, bits []int) {
}

func newbits(size int) []int {
	bits := make([]int, size)
	for i := range bits {
		bits[i] = -1
	}
	return bits
}

func solve(b int, reader *bufio.Reader) {
	bits := newbits(b)
	for i := 0; i < b; i++ {
		fmt.Println(i)
		bs, _ := reader.ReadString('\n')
		bit, _ := strconv.Atoi(bs)
		bits = append(bits, bit)
	}
	s := ""
	for _, bit := range bits {
		s += fmt.Sprintf("%d", bit)
	}
	fmt.Println(s)
}

func main() {
	reader := bufio.NewReader(os.Stdin)
	tbs, _ := reader.ReadString('\n')
	tbsa := strings.Split(tbs, " ")
	fmt.Println("t and b:", tbs)
	t, _ := strconv.Atoi(tbsa[0])
	b, _ := strconv.Atoi(tbsa[1])

	for i := 0; i < t; i++ {
		solve(b, reader)
	}
}
