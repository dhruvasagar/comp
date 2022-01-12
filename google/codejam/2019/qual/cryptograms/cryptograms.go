package main

import (
	"bufio"
	"fmt"
	"math/big"
	"os"
	"sort"
	"strconv"
	"strings"
)

var BigZero = big.NewInt(0)

type BigIntArray []big.Int

func (b BigIntArray) Len() int           { return len(b) }
func (b BigIntArray) Swap(i, j int)      { b[i], b[j] = b[j], b[i] }
func (b BigIntArray) Less(i, j int) bool { return b[i].Cmp(&b[j]) == -1 }

type Case struct {
	l       int
	n       big.Int
	ciphers BigIntArray
}

type Input struct {
	t     int
	cases []Case
}

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func logCase(num int, out string) {
	fmt.Printf("Case #%d: %s\n", num, out)
}

func parseInput(in []string) (r Input) {
	r.t, _ = strconv.Atoi(in[0])
	for i := 1; i < 2*r.t; i += 2 {
		nl := strings.Split(in[i], " ")
		nb := new(big.Int)
		nb.SetString(nl[0], 10)
		l, _ := strconv.Atoi(nl[1])
		c := Case{
			n: *nb,
			l: l,
		}
		cs := strings.Split(in[i+1], " ")
		for _, csi := range cs {
			cb := new(big.Int)
			cb.SetString(csi, 10)
			c.ciphers = append(c.ciphers, *cb)
		}
		r.cases = append(r.cases, c)
	}
	return r
}

func cipherKey(primes []big.Int) map[string]string {
	r := make(map[string]string)
	for i, p := range primes {
		r[p.String()] = string('A' + i)
	}
	return r
}

func gcd(a, b big.Int) big.Int {
	if b.Cmp(BigZero) == 0 {
		return a
	}
	t := new(big.Int)
	return gcd(b, *t.Mod(&a, &b))
}

func solve(c Case) string {
	primesMap := make(map[string]bool)
	for i := 0; i < (c.l - 1); i++ {
		c1, c2 := c.ciphers[i], c.ciphers[i+1]
		t := gcd(c1, c2)
		t1 := new(big.Int)
		primesMap[t.String()] = true
		primesMap[t1.Div(&c1, &t).String()] = true
		primesMap[t1.Div(&c2, &t).String()] = true
	}
	primes := BigIntArray{}
	for k, _ := range primesMap {
		kb := new(big.Int)
		kb.SetString(k, 10)
		primes = append(primes, *kb)
	}
	sort.Sort(primes)
	ckey := cipherKey(primes)
	r := ""
	for i := 0; i < c.l; i++ {
		var p big.Int
		t := new(big.Int)
		var c1, c2 big.Int
		c1 = c.ciphers[i]
		if (i + 1) < len(c.ciphers) {
			c2 = c.ciphers[i+1]
		}
		for j := 0; j < len(primes); j++ {
			pi := primes[j]
			if t.Mod(&c1, &pi).Cmp(BigZero) == 0 &&
				(c2.Cmp(BigZero) == 0 || t.Mod(&c2, &pi).Cmp(BigZero) == 0) {
				p = primes[j]
				break
			}
		}
		if len(r) < 1 || string(r[len(r)-1]) != ckey[t.Div(&c1, &p).String()] {
			r += ckey[t.Div(&c1, &p).String()]
		}
		if len(r) < 2 || string(r[len(r)-2]) != ckey[p.String()] {
			r += ckey[p.String()]
		}
	}
	return r
}

func main() {
	in := parseInput(readInput())
	for i, c := range in.cases {
		out := solve(c)
		logCase(i+1, out)
	}
}
