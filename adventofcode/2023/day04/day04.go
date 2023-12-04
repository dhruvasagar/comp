package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
	"time"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

type Card struct {
	id    int
	wnums []string
	mnums []string
}

func (c *Card) won() int {
	win := 0
	for _, mnum := range c.mnums {
		for _, wnum := range c.wnums {
			if wnum == mnum {
				win++
				break
			}
		}
	}
	return win
}

func (c *Card) score() int {
	win := c.won()
	return int(math.Pow(2.0, float64(win-1)))
}

func completed(unprocessed map[int]int) bool {
	t := 0
	for _, v := range unprocessed {
		t += v
	}
	return t == 0
}

func play(cards []Card) int {
	chash := make(map[int]Card)
	whash := make(map[int]int)
	for _, card := range cards {
		chash[card.id] = card
		whash[card.id] = card.won()
	}
	queue := []int{}
	for _, card := range cards {
		queue = append(queue, card.id)
	}
	r := 0
	for len(queue) != 0 {
		r += 1
		top := queue[0]
		tcard := chash[top]
		queue = queue[1:]
		for i := tcard.id + 1; i < tcard.id+1+whash[top]; i++ {
			queue = append(queue, i)
		}
	}
	return r
}

func parseCard(line string) Card {
	cnums := strings.Split(line, ":")
	id, _ := strconv.Atoi(strings.Trim(cnums[0][5:], " "))
	nums := strings.Split(cnums[1], "|")
	wnums := strings.Fields(nums[0])
	mnums := strings.Fields(nums[1])
	return Card{id, wnums, mnums}
}

func parseCards(lines []string) []Card {
	cards := []Card{}
	for _, line := range lines {
		card := parseCard(line)
		cards = append(cards, card)
	}
	return cards
}

func part1(cards []Card) int {
	r := 0
	for _, card := range cards {
		r += card.score()
	}
	return r
}

func part2(cards []Card) int {
	// fmt.Println(cards)
	// return 0
	return play(cards)
}

func main() {
	s := time.Now()
	lines := readInput()
	cards := parseCards(lines)
	s1 := time.Now()
	fmt.Println(part1(cards))
	e1 := time.Since(s1)
	s2 := time.Now()
	fmt.Println(part2(cards))
	e2 := time.Since(s2)
	e := time.Since(s)
	fmt.Printf("Time for part1: %s, part2: %s, total: %s\n", e1, e2, e)
}
