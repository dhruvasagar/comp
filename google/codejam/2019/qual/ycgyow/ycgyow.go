package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Node struct {
	x, y int // coordinates
}

func (n Node) EastNode() Node {
	return Node{
		x: n.x + 1,
		y: n.y,
	}
}

func (n Node) SouthNode() Node {
	return Node{
		x: n.x,
		y: n.y + 1,
	}
}

func (n Node) CanMoveEast(gridSize int) bool {
	return n.x < gridSize-1
}

func (n Node) CanMoveSouth(gridSize int) bool {
	return n.y < gridSize-1
}

func (n Node) IsEastOf(node Node) bool {
	return n == node.EastNode()
}

func (n Node) IsSouthOf(node Node) bool {
	return n == node.SouthNode()
}

type Edge struct {
	fnode Node // From Node
	tnode Node // To Node
}

func (e Edge) IsSouth() bool {
	return e.tnode.y > e.fnode.y
}

func (e Edge) IsEast() bool {
	return e.tnode.x > e.fnode.x
}

func (e Edge) Direction() string {
	if e.IsEast() {
		return "E"
	}
	return "S"
}

type Path struct {
	path        string
	route       []Edge
	routeMap    map[Node]Edge
	revRouteMap map[Node]Edge
}

func (p Path) String() string {
	if p.path != "" {
		return p.path
	}
	s := ""
	for _, r := range p.route {
		s += r.Direction()
	}
	return s
}

func (p *Path) AddEdge(edge Edge) {
	p.route = append(p.route, edge)
	if p.routeMap == nil {
		p.routeMap = make(map[Node]Edge)
	}
	p.routeMap[edge.fnode] = edge
}

func (p *Path) RemoveLastEdge() {
	ledge := p.route[len(p.route)-1]
	p.route = p.route[:len(p.route)-1]
	delete(p.routeMap, ledge.fnode)
}

func (p Path) GetEdge(pos Node) *Edge {
	if e, ok := p.routeMap[pos]; ok {
		return &e
	}
	return nil
}

func (p Path) GetRevEdge(pos Node) *Edge {
	if e, ok := p.revRouteMap[pos]; ok {
		return &e
	}
	return nil
}

func (p Path) DirectionChanged(edge Edge) bool {
	pedge := p.GetRevEdge(edge.fnode)
	return pedge.Direction() != edge.Direction()
}

func (p Path) Complete(gridSize int) bool {
	// Has path reached the end of the maze
	if len(p.route) == 0 {
		return false
	}
	return p.route[len(p.route)-1].tnode == Node{x: gridSize - 1, y: gridSize - 1}
}

func (p Path) GetLastEdge() Edge {
	return p.route[len(p.route)-1]
}

func NewPath(path []string) (r Path) {
	i, j := 0, 0
	r.path = strings.Join(path, "")
	r.routeMap = make(map[Node]Edge)
	r.revRouteMap = make(map[Node]Edge)
	for _, p := range path {
		ii, jj := i, j
		if p == "E" {
			i++
		} else {
			j++
		}
		fnode := Node{x: ii, y: jj}
		tnode := Node{x: i, y: j}
		edge := Edge{
			fnode,
			tnode,
		}
		r.route = append(r.route, edge)
		r.routeMap[fnode] = edge
		r.revRouteMap[tnode] = edge
	}
	return r
}

type Grid struct {
	size int
	path Path
}

func (g Grid) String() string {
	return fmt.Sprintf("%s", g.path)
}

func (g Grid) PrettyPrint() {
	fmt.Println()
	for j := 0; j < g.size; j++ {
		for i := 0; i < g.size; i++ {
			node := Node{x: i, y: j}
			edge := g.path.GetEdge(node)
			if edge == nil {
				if i == g.size-1 && j == g.size-1 {
					fmt.Print("| -x ")
				} else {
					fmt.Print("|    ")
				}
			} else {
				if edge.IsEast() {
					fmt.Print("| -> ")
				} else if edge.IsSouth() {
					fmt.Print("| -v ")
				}
			}
		}
		fmt.Print("|")
		fmt.Println()
	}
	fmt.Println()
}

func (g Grid) IsOnBorder(node Node) bool {
	return node.x == (g.size-1) || node.y == (g.size-1)
}

func (g Grid) Complete() bool {
	return g.path.GetLastEdge().tnode == Node{x: g.size - 1, y: g.size - 1}
}

type Input struct {
	t     int
	grids []Grid
}

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func logCase(num int, out fmt.Stringer) {
	fmt.Printf("Case #%d: %s\n", num, out)
}

func parseInput(in []string) (r Input) {
	r.t, _ = strconv.Atoi(in[0])
	for i := 1; i <= 2*r.t; i += 2 {
		n, _ := strconv.Atoi(in[i])
		p := strings.Split(in[i+1], "")
		grid := Grid{
			size: n,
			path: NewPath(p),
		}
		r.grids = append(r.grids, grid)
	}
	return r
}

var InvalidPathError = errors.New("Invalid Path")

func walk(g Grid, path *Path, node Node, walkedEdges map[Edge]bool) (*Path, error) {
	if path.Complete(g.size) {
		return path, nil
	}
	var err error
	var nnode Node
	var nedge Edge
	lydiaPath := g.path
	move := lydiaPath.GetEdge(node)
	if move != nil {
		// Lydia chose a path from this node which we cannot choose, choose other
		if move.IsEast() {
			if node.CanMoveSouth(g.size) {
				nnode = node.SouthNode()
				nedge = Edge{
					fnode: node,
					tnode: nnode,
				}
				if _, ok := walkedEdges[nedge]; ok {
					return path, InvalidPathError
				}
			} else {
				return path, InvalidPathError
			}
		} else if move.IsSouth() {
			if node.CanMoveEast(g.size) {
				nnode = node.EastNode()
				nedge = Edge{
					fnode: node,
					tnode: nnode,
				}
				if _, ok := walkedEdges[nedge]; ok {
					return path, InvalidPathError
				}
			} else {
				return path, InvalidPathError
			}
		}
	} else {
		if node.CanMoveEast(g.size) {
			nnode = node.EastNode()
			nedge = Edge{
				fnode: node,
				tnode: nnode,
			}
			if _, ok := walkedEdges[nedge]; ok {
				if node.CanMoveSouth(g.size) {
					nnode = node.SouthNode()
					nedge = Edge{
						fnode: node,
						tnode: nnode,
					}
					if _, ok := walkedEdges[nedge]; ok {
						return path, InvalidPathError
					}
				} else {
					return path, InvalidPathError
				}
			}
		}
		if node.CanMoveSouth(g.size) {
			nnode = node.SouthNode()
			nedge = Edge{
				fnode: node,
				tnode: nnode,
			}
			if _, ok := walkedEdges[nedge]; ok {
				if node.CanMoveEast(g.size) {
					nnode = node.EastNode()
					nedge = Edge{
						fnode: node,
						tnode: nnode,
					}
					if _, ok := walkedEdges[nedge]; ok {
						return path, InvalidPathError
					}
				} else {
					return path, InvalidPathError
				}
			}
		}
	}
	nedge = Edge{
		fnode: node,
		tnode: nnode,
	}
	path.AddEdge(nedge)
	walkedEdges[nedge] = true
	path, err = walk(g, path, nnode, walkedEdges)
	if err != nil {
		path.RemoveLastEdge()
		return walk(g, path, node, walkedEdges)
	}
	return path, nil
}

func solve(g Grid) Grid {
	p := &Path{}
	node := Node{x: 0, y: 0}
	walkedEdges := make(map[Edge]bool)
	p, _ = walk(g, p, node, walkedEdges)
	r := Grid{size: g.size}
	r.path = *p
	return r
}

func main() {
	in := parseInput(readInput())
	for i, grid := range in.grids {
		logCase(i+1, solve(grid))
	}
}
