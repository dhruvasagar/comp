package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"github.com/dhruvasagar/adventofcode/2019/util"
)

const (
	ore  string = "ORE"
	fuel string = "FUEL"
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

func (r Reactants) needed(quantity int) Reactants {
	res := Reactants{}
	for _, rr := range r {
		c := Chemical{rr.name, rr.quantity * quantity}
		res = append(res, c)
	}
	return res
}

type Reaction struct {
	reactants Reactants
	product   Chemical
}

func (r Reaction) getReactant(name string) *Chemical {
	for _, reactant := range r.reactants {
		if reactant.name == name {
			return &reactant
		}
	}
	return nil
}

func (r Reaction) hasOreReactant() bool {
	return r.getReactant(ore) != nil
}

func (r Reaction) oreNeeded(productQuantity int) int {
	if !r.hasOreReactant() {
		return -1
	}
	oreReactant := r.getReactant(ore)
	if productQuantity < r.product.quantity {
		return oreReactant.quantity
	} else {
		return int(math.Ceil(float64(productQuantity)/float64(r.product.quantity))) * oreReactant.quantity
	}
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

type ChainReaction struct {
	reactions []Reaction

	prMap map[string]Reaction
}

func NewChainReaction(reactions []Reaction) ChainReaction {
	cr := ChainReaction{
		reactions: reactions,
		prMap:     make(map[string]Reaction),
	}
	for _, r := range reactions {
		cr.prMap[r.product.signature()] = r
	}
	return cr
}

func (c ChainReaction) flatten(chemical Chemical) Reactants {
	reaction := c.prMap[chemical.name]
	if reaction.hasOreReactant() {
		return []Chemical{chemical}
	}
	var reactants Reactants
	if chemical.quantity < reaction.product.quantity {
		reactants = reaction.reactants
	} else {
		fmt.Printf("chemical: %+v, product: %+v\n", chemical, reaction.product)
		quantityNeeded := int(math.Ceil(float64(chemical.quantity) / float64(reaction.product.quantity)))
		reactants = reaction.reactants.needed(quantityNeeded)
	}
	flattendReactants := Reactants{}
	for _, r := range reactants {
		flattendReactants = append(flattendReactants, c.flatten(r)...)
	}
	return flattendReactants
}

func (c ChainReaction) oreNeeded(chemical Chemical) int {
	chemicals := c.flatten(chemical)
	fmt.Println(chemicals)
	chemMap := make(map[string]int)
	for _, chem := range chemicals {
		chemSums, ok := chemMap[chem.signature()]
		if !ok {
			chemMap[chem.signature()] = chem.quantity
		} else {
			chemSums += chem.quantity
			chemMap[chem.signature()] = chemSums
		}
	}
	fmt.Println(chemMap)
	res := 0
	for name, quantity := range chemMap {
		reaction := c.prMap[name]
		res += reaction.oreNeeded(quantity)
	}
	return res
}

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

func parseReaction(reaction string) Reaction {
	rpparts := strings.Split(reaction, "=>")
	reactants := parseReactants(strings.Trim(rpparts[0], " "))
	product := parseProduct(strings.Trim(rpparts[1], " "))
	return NewReaction(reactants, product)
}

func parseInput(lines []string) ChainReaction {
	r := []Reaction{}
	for _, line := range lines {
		reaction := parseReaction(line)
		r = append(r, reaction)
	}
	return NewChainReaction(r)
}

func main() {
	chainReaction := parseInput(util.ReadLines())
	fmt.Println(chainReaction)
}
