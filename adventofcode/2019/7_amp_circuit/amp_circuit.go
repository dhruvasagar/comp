package main

import (
	"fmt"

	"github.com/dhruvasagar/adventofcode/2019/util"
)

/*Silver - Part 1 */
func Silver() {
	instructions := util.ReadIntcodeInstructions()

	amps := make([]*util.Computer, 5)
	var maxSignal int
	combos := getPossibleCombos(0)
	for _, combo := range combos {
		// remake the amps for each combo, i dont really want to deal with resetting instructions, input, and output
		for i := 0; i < 5; i++ {
			amps[i] = util.NewComputer(instructions, i)
			defer amps[i].Close()
			amps[i].Type(combo[i])
			go amps[i].Run()
		}

		var prevAmp *util.Computer
		// fmt.Println("Before for loop")
		for _, curAmp := range amps {
			go provideInput(curAmp, prevAmp, 0)
			prevAmp = curAmp
		}

		// Get the signal from the last amp and compare it to the max
		out, ok := amps[4].Read()

		if ok && out > maxSignal {
			maxSignal = out
		}
	}

	fmt.Printf("Max possible signal: %d \n", maxSignal)
}

/*Gold - Part 2 */
func Gold() {
	amps := make([]*util.Computer, 5)
	var maxSignal int
	combos := getPossibleCombos(5)

	for _, combo := range combos {
		// remake the amps for each combo, i dont really want to deal with resetting instructions, input, and output
		for i := 0; i < 5; i++ {
			amps[i] = util.NewComputer(util.ReadIntcodeInstructions(), i)
			amps[i].Type(combo[i])
			defer amps[i].Close()
			go amps[i].Run()
		}

		var out, outTemp int
		ok := true
		for ok {
			var prevAmp *util.Computer
			for _, curAmp := range amps {
				go provideInput(curAmp, prevAmp, out)
				prevAmp = curAmp
			}

			outTemp, ok = amps[4].Read()
			if ok {
				out = outTemp
			}
		}

		if out > maxSignal {
			maxSignal = out
		}
	}

	fmt.Printf("Max possible signal in feedback loop: %d \n", maxSignal)
}

func provideInput(amp, prevAmp *util.Computer, initial int) {
	if prevAmp != nil {
		out, ok := prevAmp.Read()
		if !ok {
			return
		}
		amp.Type(out)
	} else {
		amp.Type(initial)
	}
}

func getPossibleCombos(offset int) [][]int {
	combos := [][]int{}
	for a := 0 + offset; a < 5+offset; a++ {
		for b := 0 + offset; b < 5+offset; b++ {
			for c := 0 + offset; c < 5+offset; c++ {
				for d := 0 + offset; d < 5+offset; d++ {
					for e := 0 + offset; e < 5+offset; e++ {
						tester := map[int]bool{}
						tester[a] = true
						tester[b] = true
						tester[c] = true
						tester[d] = true
						tester[e] = true
						if len(tester) == 5 {
							combos = append(combos, []int{a, b, c, d, e})
						}
					}
				}
			}
		}
	}
	return combos
}

func main() {
	Silver()
	Gold()
}
