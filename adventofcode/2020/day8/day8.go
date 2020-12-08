package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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

const (
	acc = "acc"
	jmp = "jmp"
	nop = "nop"
)

type Instruction struct {
	operation string
	argument  int
}

func (i *Instruction) String() string {
	return fmt.Sprintf("%s-%d", i.operation, i.argument)
}

type Register = int

type Program struct {
	ip  int
	acc Register

	_ins         []*Instruction // BackUp for reset
	instructions []*Instruction
}

func cloneInstructions(src []*Instruction, dst []*Instruction) {
	for i, in := range src {
		cin := *in
		dst[i] = &cin
	}
}

func NewProgram(instructions []*Instruction) *Program {
	p := &Program{instructions: instructions}
	p._ins = make([]*Instruction, len(instructions))
	cloneInstructions(instructions, p._ins)
	return p
}

func (p *Program) Reset() {
	p.ip = 0
	p.acc = 0
	cloneInstructions(p._ins, p.instructions)
}

func (p *Program) HasInfiniteLoop() bool {
	repeatInstruction := make(map[string]bool)
	for p.ip < len(p.instructions) {
		instruction := p.instructions[p.ip]
		key := fmt.Sprintf("%d-%s", p.ip, instruction)
		if _, ok := repeatInstruction[key]; ok {
			return true
		}
		switch instruction.operation {
		case acc:
			p.acc += instruction.argument
			p.ip++
		case jmp:
			p.ip += instruction.argument
		case nop:
			p.ip++
		}
		repeatInstruction[key] = true
	}
	return false
}

func (p *Program) ExecuteBeforeLoop() {
	repeatInstruction := make(map[string]bool)
	for p.ip < len(p.instructions) {
		instruction := p.instructions[p.ip]
		key := fmt.Sprintf("%d-%s", p.ip, instruction)
		if _, ok := repeatInstruction[key]; ok {
			break
		}
		switch instruction.operation {
		case acc:
			p.acc += instruction.argument
			p.ip++
		case jmp:
			p.ip += instruction.argument
		case nop:
			p.ip++
		}
		repeatInstruction[key] = true
	}
}

func (p *Program) String() string {
	r := fmt.Sprintln("Program: ")
	for _, in := range p.instructions {
		r += fmt.Sprintln(in)
	}
	r += fmt.Sprintln("copied instructions:")
	for _, in := range p._ins {
		r += fmt.Sprintln(in)
	}
	return r
}

func part1(program *Program) string {
	program.ExecuteBeforeLoop()
	return fmt.Sprintf("%d", program.acc)
}

func part2(p *Program) string {
	for {
		for i := range p._ins {
			in := p.instructions[i]
			if in.operation == nop {
				in.operation = jmp
			} else if in.operation == jmp {
				in.operation = nop
			} else {
				continue
			}
			if !p.HasInfiniteLoop() {
				return fmt.Sprint(p.acc)
			}
			p.Reset()
		}
	}
}

func parseInput(in []string) *Program {
	instructions := []*Instruction{}
	for _, ip := range in {
		oparg := strings.Split(ip, " ")
		arg, _ := strconv.Atoi(oparg[1])
		instruction := &Instruction{operation: oparg[0], argument: arg}
		instructions = append(instructions, instruction)
	}
	return NewProgram(instructions)
}

func main() {
	program := parseInput(readInput())
	fmt.Println(part1(program))
	fmt.Println(part2(program))
}
