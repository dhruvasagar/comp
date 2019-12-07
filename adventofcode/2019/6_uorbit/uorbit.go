package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type node struct {
	name string

	parent   *node
	children []*node
}

func (n node) log() string {
	pname := ""
	if n.parent != nil {
		pname = n.parent.name
	}
	return fmt.Sprintf("name: %s, parent: %s\n", n.name, pname)
}

func (n node) String() string {
	st := ""
	if len(n.children) == 0 {
		// leaf node
		return fmt.Sprintf("%s", n.name)
	}
	for _, child := range n.children {
		st = st + fmt.Sprintf("%s -> %s ", n.name, child)
	}
	return st
}

func (n node) checksum() int {
	checksum := 0
	if n.parent == nil {
		return checksum
	}
	tn := n
	for tn.parent != nil {
		checksum++
		tn = *tn.parent
	}
	return checksum
}

func newTree(nodemaps [][]string) (*node, map[string]*node) {
	cache := make(map[string]*node)

	// Prepare COM node since it may not necessarily be the first one
	name := "COM"
	root := &node{name: name}
	cache[name] = root

	for _, nodemap := range nodemaps {
		var n1, n2 *node
		if n, ok := cache[nodemap[0]]; ok {
			n1 = n
		}
		if n, ok := cache[nodemap[1]]; ok {
			n2 = n
		}
		if n1 == nil {
			n1 = &node{name: nodemap[0]}
			cache[nodemap[0]] = n1
		}
		if n2 == nil {
			n2 = &node{name: nodemap[1]}
			cache[nodemap[1]] = n2
		}
		n1.children = append(n1.children, n2)
		n2.parent = n1
	}
	return root, cache
}

func checksum(nodemap map[string]*node) int {
	sum := 0
	for _, node := range nodemap {
		sum += node.checksum()
	}
	return sum
}

func minOrbitalTransferCount(nodemap map[string]*node) int {
	you := "YOU"
	san := "SAN"
	younode := nodemap[you]
	sannode := nodemap[san]
	youHeirarchy := []*node{}
	tn := younode.parent
	var commonAncestor *node
	for tn.parent != nil {
		youHeirarchy = append(youHeirarchy, tn)
		tn = tn.parent
	}
	youlength := 0
	sanlength := 0
	tn = sannode.parent
	for tn.parent != nil {
		youlength = 0
		for _, youp := range youHeirarchy {
			if youp == tn {
				commonAncestor = tn
				break
			}
			youlength++
		}
		if commonAncestor != nil {
			break
		}
		tn = tn.parent
		sanlength++
	}
	return youlength + sanlength
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	nodemaps := [][]string{}
	for scanner.Scan() {
		nm := scanner.Text()
		ab := strings.Split(nm, ")")
		nodemaps = append(nodemaps, []string{ab[0], ab[1]})
	}
	_, nodemap := newTree(nodemaps)
	fmt.Println(checksum(nodemap))
	fmt.Println(minOrbitalTransferCount(nodemap))
}
