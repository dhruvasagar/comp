package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
)

const (
	ADD  = '+'
	MUL  = '*'
	PARL = '('
	PARR = ')'
)

func precedence(r rune) int {
	switch r {
	case ADD:
		return 2
	case MUL:
		return 1
	}
	return 0
}

func evalOp(a, b int, op rune) int {
	switch op {
	case ADD:
		return a + b
	case MUL:
		return a * b
	}
	return 0
}

func evalOpOnStack(ops *Stack, vals *Stack) {
	val2 := vals.pop()
	val1 := vals.pop()
	op := ops.pop()

	vals.push(evalOp(val1, val2, rune(op)))
}

type Stack struct {
	values []int
}

func (s *Stack) empty() bool {
	return len(s.values) == 0
}

func (s *Stack) top() int {
	return s.values[len(s.values)-1]
}

func (s *Stack) pop() int {
	top := s.top()
	s.values = s.values[:len(s.values)-1]
	return top
}

func (s *Stack) push(a int) {
	s.values = append(s.values, a)
}

var digitRegexp = regexp.MustCompile(`^\d$`)

func isDigit(r byte) bool {
	return digitRegexp.MatchString(string(r))
}

func evaluate(tokens string) int {
	ops := &Stack{}
	vals := &Stack{}

	for i := 0; i < len(tokens); i++ {
		if tokens[i] == ' ' {
			continue
		}

		if isDigit(tokens[i]) {
			num := 0
			for ; i < len(tokens) && isDigit(tokens[i]); i++ {
				num = num*10 + int(tokens[i]-'0')
			}
			vals.push(num)
			i--
		}

		if tokens[i] == PARL {
			ops.push(int(tokens[i]))
		}

		if tokens[i] == PARR {
			for !ops.empty() && ops.top() != PARL {
				evalOpOnStack(ops, vals)
			}

			if !ops.empty() {
				ops.pop()
			}
		}

		if tokens[i] == ADD || tokens[i] == MUL {
			for !ops.empty() && precedence(rune(ops.top())) >= precedence(rune(tokens[i])) {
				evalOpOnStack(ops, vals)
			}

			ops.push(int(tokens[i]))
		}
	}

	for !ops.empty() {
		evalOpOnStack(ops, vals)
	}

	return vals.top()
}

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func main() {
	exprs := readInput()
	sum := 0
	for _, expr := range exprs {
		sum += evaluate(expr)
	}
	fmt.Println(sum)
}
