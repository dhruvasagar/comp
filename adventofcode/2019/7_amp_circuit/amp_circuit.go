package main

import "fmt"

type Opcode int

const (
	add = iota + 1
	mul
	inp
	out
	jmpt
	jmpf
	lt
	eql
	hlt = 99
)

type Instruction struct {
	opcode Opcode
	size   int
}

var (
	addInstruction = Instruction{
		opcode: add,
		size:   4,
	}
	mulInstruction = Instruction{
		opcode: mul,
		size:   4,
	}
	inpInstruction = Instruction{
		opcode: inp,
		size:   2,
	}
	outInstruction = Instruction{
		opcode: out,
		size:   2,
	}
	jmptInstruction = Instruction{
		opcode: jmpt,
		size:   3,
	}
	jmpfInstruction = Instruction{
		opcode: jmpf,
		size:   3,
	}
	ltInstruction = Instruction{
		opcode: lt,
		size:   4,
	}
	eqlInstruction = Instruction{
		opcode: eql,
		size:   4,
	}
)

type Program struct {
	name string
	pos  int
	code []int

	in   chan int
	out  chan int
	done chan bool
}

func parseOpcode(opcode int) (Opcode, []int) {
	if opcode < 99 {
		return Opcode(opcode), []int{0, 0}
	}
	op := Opcode(opcode % 100)
	modeint := opcode / 100
	modes := []int{}
	for n := modeint; n > 0; n /= 10 {
		modes = append(modes, n%10)
	}
	if len(modes) < 2 {
		modes = append(modes, 0)
	}
	return op, modes
}

func (p Program) log(format string, msg ...interface{}) {
	fmt.Printf("%s: %s\n", p.name, fmt.Sprintf(format, msg...))
}

func (p Program) getParam(pos, mode int) int {
	if mode == 0 {
		return p.code[p.code[pos]]
	}
	return p.code[pos]
}

func (p *Program) executeInstruction() {
	opcode, modes := parseOpcode(p.code[p.pos])
	// p.log("pos: %d, opcode: %d\n", p.pos, opcode)
	switch opcode {
	case add:
		p1 := p.getParam(p.pos+1, modes[0])
		p2 := p.getParam(p.pos+2, modes[1])
		p.code[p.code[p.pos+3]] = p1 + p2
		p.pos += addInstruction.size
	case mul:
		p1 := p.getParam(p.pos+1, modes[0])
		p2 := p.getParam(p.pos+2, modes[1])
		p.code[p.code[p.pos+3]] = p1 * p2
		p.pos += mulInstruction.size
	case inp:
		input := <-p.in
		p.log("input: %d", input)
		p.code[p.code[p.pos+1]] = input
		p.pos += inpInstruction.size
	case out:
		output := p.code[p.code[p.pos+1]]
		p.log("output: %d", output)
		p.out <- output
		p.pos += outInstruction.size
	case jmpt:
		p1 := p.getParam(p.pos+1, modes[0])
		p2 := p.getParam(p.pos+2, modes[1])
		if p1 > 0 {
			p.pos = p2
		} else {
			p.pos += jmptInstruction.size
		}
	case jmpf:
		p1 := p.getParam(p.pos+1, modes[0])
		p2 := p.getParam(p.pos+2, modes[1])
		if p1 == 0 {
			p.pos = p2
		} else {
			p.pos += jmpfInstruction.size
		}
	case lt:
		p1 := p.getParam(p.pos+1, modes[0])
		p2 := p.getParam(p.pos+2, modes[1])
		if p1 < p2 {
			p.code[p.code[p.pos+3]] = 1
		} else {
			p.code[p.code[p.pos+3]] = 0
		}
		p.pos += ltInstruction.size
	case eql:
		p1 := p.getParam(p.pos+1, modes[0])
		p2 := p.getParam(p.pos+2, modes[1])
		if p1 == p2 {
			p.code[p.code[p.pos+3]] = 1
		} else {
			p.code[p.code[p.pos+3]] = 0
		}
		p.pos += eqlInstruction.size
	case hlt:
		fmt.Printf("%s: Program Done!\n", p.name)
		p.done <- true
	}
}

func (p *Program) execute() {
	go func() {
		for {
			p.executeInstruction()
		}
	}()
	<-p.done
}

func NewProgram(name string, code []int) *Program {
	p := Program{
		name: name,
		code: code,
		in:   make(chan int, 2),
		out:  make(chan int, 1),
		done: make(chan bool),
	}
	return &p
}

func NewCircuit(code, phases []int) int {
	si := 0

	for i, phase := range phases {
		name := fmt.Sprintf("amp_%d", i)
		p := NewProgram(name, code)
		p.in <- phase
		p.in <- si
		go p.execute()
		out := <-p.out
		si = out
	}

	return si
}

// Perm calls f with each permutation of a.
func Perm(a []int, f func([]int)) {
	perm(a, f, 0)
}

// Permute the values at index i to len(a)-1.
func perm(a []int, f func([]int), i int) {
	if i > len(a) {
		f(a)
		return
	}
	perm(a, f, i+1)
	for j := i + 1; j < len(a); j++ {
		a[i], a[j] = a[j], a[i]
		perm(a, f, i+1)
		a[i], a[j] = a[j], a[i]
	}
}

func findMax(code, phases_range []int) int {
	outputs := []int{}
	Perm(phases_range, func(test []int) {
		// fmt.Println("test phase: ", test)
		outputs = append(outputs, NewCircuit(code, test))
	})
	max := 0
	for _, output := range outputs {
		if max < output {
			max = output
		}
	}
	return max
}

func main() {
	code := []int{3, 8, 1001, 8, 10, 8, 105, 1, 0, 0, 21, 42, 67, 84, 97, 118, 199, 280, 361, 442, 99999, 3, 9, 101, 4, 9, 9, 102, 5, 9, 9, 101, 2, 9, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 101, 5, 9, 9, 102, 5, 9, 9, 1001, 9, 5, 9, 102, 3, 9, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 1001, 9, 5, 9, 1002, 9, 2, 9, 1001, 9, 5, 9, 4, 9, 99, 3, 9, 1001, 9, 5, 9, 1002, 9, 3, 9, 4, 9, 99, 3, 9, 102, 4, 9, 9, 101, 4, 9, 9, 102, 2, 9, 9, 101, 3, 9, 9, 4, 9, 99, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 99, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99}
	// code := []int{3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10}
	// phases := []int{9, 7, 8, 5, 6}
	fmt.Println(findMax(code, []int{0, 1, 2, 3, 4}))
	// fmt.Println(findMax(code, []int{5, 6, 7, 8, 9}))
	// fmt.Println(NewCircuit(code, phases))
}
