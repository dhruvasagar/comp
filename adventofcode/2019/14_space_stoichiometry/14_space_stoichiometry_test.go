package main

import "testing"

func TestParseChemical(t *testing.T) {
	data := []struct {
		in  string
		res Chemical
	}{
		{"10 ORE", Chemical{"ORE", 10}},
		{"7 A", Chemical{"A", 7}},
		{"1 B", Chemical{"B", 1}},
		{"1 B ", Chemical{"B", 1}},
	}
	for _, d := range data {
		res := parseChemical(d.in)
		if res != d.res {
			t.Errorf("Chemical parsing failed, expected %v, got %v", d.res, res)
		}
	}
}

func TestParseReactants(t *testing.T) {
	data := []struct {
		in        string
		reactants Reactants
	}{
		{"7 A, 1 B", Reactants{Chemical{"A", 7}, Chemical{"B", 1}}},
		{"7 A, 1 C", Reactants{Chemical{"A", 7}, Chemical{"C", 1}}},
		{"7 A, 1 D", Reactants{Chemical{"A", 7}, Chemical{"D", 1}}},
		{"7 A, 1 E ", Reactants{Chemical{"A", 7}, Chemical{"E", 1}}},
	}
	for _, d := range data {
		res := parseReactants(d.in)
		if len(res) != 2 || res[0] != d.reactants[0] || res[1] != d.reactants[1] {
			t.Errorf("Reactants parsing failed, expected %v, got %v", d.reactants, res)
		}
	}
}

func TestParseReaction(t *testing.T) {
	data := []struct {
		in      string
		rsize   int
		product Chemical
	}{
		{"10 ORE => 10 A", 1, Chemical{"A", 10}},
		{"1 ORE => 1 B", 1, Chemical{"B", 1}},
		{"7 A, 1 B => 1 C", 2, Chemical{"C", 1}},
		{"7 A, 1 C => 1 D", 2, Chemical{"D", 1}},
		{"7 A, 1 D => 1 E", 2, Chemical{"E", 1}},
		{"7 A , 1 E  =>  1 FUEL", 2, Chemical{"FUEL", 1}},
	}
	for _, d := range data {
		res := parseReaction(d.in)
		if len(res.reactants) != d.rsize || res.product != d.product {
			t.Errorf("Reaction parsing failed, got %v", res)
		}
	}
}

func TestReactantsNeeded(t *testing.T) {
	data := []struct {
		in       string
		quantity int
	}{
		{"7 A, 1 B", 2},
		{"7 A, 1 B", 4},
	}
	for _, d := range data {
		reactants := parseReactants(d.in)
		nreactants := reactants.needed(d.quantity)
		if len(nreactants) != len(reactants) {
			t.Errorf("Reactants needed failed, expected to be same length as reactants, got :%d", len(nreactants))
		}
		for i, r := range nreactants {
			expQuantity := reactants[i].quantity * d.quantity
			if r.quantity != expQuantity {
				t.Errorf("Reactants needed failed, expected quantity %d, got %d", expQuantity, r.quantity)
			}
		}
	}
}

func TestReactionOreNeeded(t *testing.T) {
	data := []struct {
		in        string
		quantity  int
		oreNeeded int
	}{
		{"9 ORE => 2 A", 2, 9},
		{"9 ORE => 2 A", 1, 9},
		{"9 ORE => 2 A", 3, 18},
	}
	for _, d := range data {
		reaction := parseReaction(d.in)
		res := reaction.oreNeeded(d.quantity)
		if res != d.oreNeeded {
			t.Errorf("Reaction Ore Needed failed, expected %d, got %d", d.oreNeeded, res)
		}
	}
}

func TestFlatten(t *testing.T) {
	data := []struct {
		in   []string
		name string
		res  int
	}{
		{[]string{
			"10 ORE => 10 A",
		}, "A", 1},
		{[]string{
			"10 ORE => 10 A",
			"7 A => 1 FUEL",
		}, "FUEL", 1},
		{[]string{
			"10 ORE => 10 A",
			"1 ORE => 1 B",
			"20 A, 1 B => 1 C",
		}, "C", 2},
	}
	for _, d := range data {
		cr := parseInput(d.in)
		res := cr.flatten(Chemical{d.name, 1})
		if len(res) != d.res {
			t.Errorf("Flatten failed, expected %v, got %v", d.res, res)
		}
	}
}

func TestChainReactionOreNeeded(t *testing.T) {
	data := []struct {
		in  []string
		res int
	}{
		{[]string{
			"10 ORE => 10 A",
			"7 A => 1 FUEL",
		}, 10},
		{[]string{
			"10 ORE => 10 A",
			"1 ORE => 1 B",
			"7 A, 1 B => 1 C",
			"7 A, 1 C => 1 D",
			"7 A, 1 D => 1 E",
			"7 A, 1 E => 1 FUEL",
		}, 31},
		{[]string{
			"9 ORE => 2 A",
			"8 ORE => 3 B",
			"7 ORE => 5 C",
			"3 A, 4 B => 1 AB",
			"5 B, 7 C => 1 BC",
			"4 C, 1 A => 1 CA",
			"2 AB, 3 BC, 4 CA => 1 FUEL",
		}, 165},
		{[]string{
			"157 ORE => 5 NZVS",
			"165 ORE => 6 DCFZ",
			"44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL",
			"12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ",
			"179 ORE => 7 PSHF",
			"177 ORE => 5 HKGWZ",
			"7 DCFZ, 7 PSHF => 2 XJWVT",
			"165 ORE => 2 GPVTF",
			"3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT",
		}, 13312},
		{[]string{
			"2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG",
			"17 NVRVD, 3 JNWZP => 8 VPVL",
			"53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL",
			"22 VJHF, 37 MNCFX => 5 FWMGM",
			"139 ORE => 4 NVRVD",
			"144 ORE => 7 JNWZP",
			"5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC",
			"5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV",
			"145 ORE => 6 MNCFX",
			"1 NVRVD => 8 CXFTF",
			"1 VJHF, 6 MNCFX => 4 RFSQX",
			"176 ORE => 6 VJHF",
		}, 180697},
		// {[]string{
		// 	"171 ORE => 8 CNZTR",
		// 	"7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL",
		// 	"114 ORE => 4 BHXH",
		// 	"14 VRPVC => 6 BMBT",
		// 	"6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL",
		// 	"6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT",
		// 	"15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW",
		// 	"13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW",
		// 	"5 BMBT => 4 WPTQ",
		// 	"189 ORE => 9 KTJDG",
		// 	"1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP",
		// 	"12 VRPVC, 27 CNZTR => 2 XDBXC",
		// 	"15 KTJDG, 12 BHXH => 5 XCVML",
		// 	"3 BHXH, 2 VRPVC => 7 MZWV",
		// 	"121 ORE => 7 VRPVC",
		// 	"7 XCVML => 6 RJRHP",
		// 	"5 BHXH, 4 VRPVC => 5 LTCX",
		// }, 2210736},
	}
	for _, d := range data {
		cr := parseInput(d.in)
		res := cr.oreNeeded(Chemical{fuel, 1})
		if res != d.res {
			t.Errorf("Chain Reaction Ore Needed failed, expected %d, got %d", d.res, res)
		}
	}
}
