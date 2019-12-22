package main

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/dhruvasagar/adventofcode/2019/util"
)

type Chemical struct {
	name     string
	quantity int
}

func (c Chemical) signature() string {
	return c.name
}

type Reactants []Chemical

func (r Reactants) signature() string {
	rs := []string{}
	for _, c := range r {
		rs = append(rs, fmt.Sprintln(c))
	}
	return strings.Join(rs, ",")
}

type Reaction struct {
	reactants Reactants
	product   Chemical

	rpMap map[string]Chemical
	prMap map[string]Reactants
}

func NewReaction(reactants Reactants, product Chemical) Reaction {
	reaction := Reaction{
		reactants: reactants,
		product:   product,
	}
	rpMap := make(map[string]Chemical)
	prMap := make(map[string]Reactants)
	rpMap[reactants.signature()] = product
	prMap[product.signature()] = reactants
	return reaction
}

type ChainReaction []Reaction

func parseChemical(chemical string) Chemical {
	qn := strings.Split(chemical, " ")
	quantity, _ := strconv.Atoi(qn[0])
	name := qn[1]
	return Chemical{
		name:     name,
		quantity: quantity,
	}
}

func parseReactants(reactants string) Reactants {
	r := Reactants{}
	chemicals := strings.Split(reactants, ",")
	for _, c := range chemicals {
		r = append(r, parseChemical(strings.Trim(c, " ")))
	}
	return r
}

func parseProduct(product string) Chemical {
	return parseChemical(product)
}

func parseReaction(rpparts []string) Reaction {
	reactants := parseReactants(strings.Trim(rpparts[0], " "))
	product := parseProduct(strings.Trim(rpparts[1], " "))
	return NewReaction(reactants, product)
}

func parseInput(lines []string) ChainReaction {
	r := ChainReaction{}
	for _, line := range lines {
		rpparts := strings.Split(line, "=>")
		reaction := parseReaction(rpparts)
		r = append(r, reaction)
	}
	return r
}

func main() {
	chainReaction := parseInput(util.ReadLines())
	fmt.Println(chainReaction)
}
