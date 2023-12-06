package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

func winCount(time int, dist int) int {
	for t := 0; t <= time; t++ {
		if dist < (t * (time - t)) {
			return 1 + time - (2 * t)
		}
	}
	return 0
}

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func part1(lines []string) int {
	ts := strings.Fields(lines[0])[1:]
	ds := strings.Fields(lines[1])[1:]
	w := 1
	for i := 0; i < len(ts); i++ {
		tn, _ := strconv.Atoi(ts[i])
		dn, _ := strconv.Atoi(ds[i])
		w *= winCount(tn, dn)
	}
	return w
}

func part2(lines []string) int {
	ts := strings.Fields(lines[0])[1:]
	ds := strings.Fields(lines[1])[1:]
	tnums := strings.Join(ts, "")
	dnums := strings.Join(ds, "")
	tnum, _ := strconv.Atoi(tnums)
	dnum, _ := strconv.Atoi(dnums)
	return winCount(tnum, dnum)
}

func main() {
	s := time.Now()
	lines := readInput()
	s1 := time.Now()
	fmt.Println(part1(lines))
	e1 := time.Since(s1)
	s2 := time.Now()
	fmt.Println(part2(lines))
	e2 := time.Since(s2)
	e := time.Since(s)
	fmt.Printf("Time for part1: %s, part2: %s, total: %s\n", e1, e2, e)
}
