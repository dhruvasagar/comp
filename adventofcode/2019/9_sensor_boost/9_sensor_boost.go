package main

import (
	"fmt"

	"github.com/dhruvasagar/comp/adventofcode/2019/util"
)

func Silver() {
	instructions := util.ReadIntcodeInstructions()
	computer := util.NewComputer(instructions, 0)
	defer computer.Close()
	computer.Type(1)
	go computer.Run()
	out, _ := computer.Read()
	fmt.Println(out)
}

func Gold() {
	instructions := util.ReadIntcodeInstructions()
	computer := util.NewComputer(instructions, 0)
	defer computer.Close()
	computer.Type(2)
	go computer.Run()
	out, _ := computer.Read()
	fmt.Println(out)
}

func main() {
	Silver()
	Gold()
}
