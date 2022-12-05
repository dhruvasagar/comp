package main

import (
	"bufio"
  "regexp"
	"fmt"
	"os"
  "strings"
  "strconv"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

type Move struct {
  count int;
  from int;
  to int;
}

func reverse(s string) string {
  runes := []rune(s)
  for i, j := 0, len(runes)-1; i < j; i, j = i + 1, j - 1 {
    runes[i], runes[j] = runes[j], runes[i]
  }
  return string(runes)
}

func (m *Move) apply(stacks []string) {
  fstr := stacks[m.from]
  stacks[m.to] = stacks[m.to] + reverse(fstr[len(fstr)-m.count:])
  stacks[m.from] = fstr[0:len(fstr)-m.count]
}

func (m *Move) apply2(stacks []string) {
  fstr := stacks[m.from]
  stacks[m.to] = stacks[m.to] + fstr[len(fstr)-m.count:]
  stacks[m.from] = fstr[0:len(fstr)-m.count]
}

func parseStacks(lines []string) ([]string, []Move) {
  sindex := 0
  for index, line := range(lines) {
    if strings.HasPrefix(strings.TrimSpace(line), "1") {
      sindex = index
      break
    }
  }
  scount := len(strings.Fields(lines[sindex]))

  stacks := []string{}
  for i := 0; i < scount; i++ {
    stack := ""
    for _, line := range(lines[0:sindex]) {
      index := i * 4 + 1
      stack = fmt.Sprintf("%c%s", line[index], stack)
    }
    stacks = append(stacks, strings.TrimSpace(stack))
  }

  moves := []Move{}
  r := regexp.MustCompile(`move (\d+) from (\d+) to (\d+)`)
  for _, line := range(lines[sindex+2:]) {
    res := r.FindAllStringSubmatch(line, -1)[0]
    count, _ := strconv.Atoi(res[1])
    from, _ := strconv.Atoi(res[2])
    to, _ := strconv.Atoi(res[3])
    move := Move {
      count: count,
      from: from - 1,
      to: to - 1,
    }
    moves = append(moves, move)
  }

  return stacks, moves
}

func part1(stacks []string, moves []Move) string {
  for _, move := range(moves) {
    move.apply(stacks)
  }
  s := ""
  for _, stack := range(stacks) {
    s = fmt.Sprintf("%s%c", s, stack[len(stack)-1])
  }
  return s
}

func part2(stacks []string, moves []Move) string {
  for _, move := range(moves) {
    move.apply2(stacks)
  }
  s := ""
  for _, stack := range(stacks) {
    s = fmt.Sprintf("%s%c", s, stack[len(stack)-1])
  }
  return s
}

func main() {
  input := readInput()

  stacks, moves := parseStacks(input)
  fmt.Println(part1(stacks, moves))

  stacks, moves = parseStacks(input)
  fmt.Println(part2(stacks, moves))
}
