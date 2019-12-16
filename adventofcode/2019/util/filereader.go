package util

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func ReadLines(day int) []string {
	fileName := fmt.Sprintf("../input/day%d.txt", day)
	file, err := os.Open(fileName)
	defer file.Close()
	if err != nil {
		fmt.Println(err)
	}

	scanner := bufio.NewScanner(file)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	return lines
}

func ReadIntcodeInstructions() []int {
	fileName := fmt.Sprintf("input")
	file, err := os.Open(fileName)
	defer file.Close()
	if err != nil {
		fmt.Println(err)
	}

	scanner := bufio.NewScanner(file)
	scanner.Split(commaSplitFunc)
	// scanner.Split(bufio.)
	var ints []int
	for scanner.Scan() {
		num, err := strconv.Atoi(strings.TrimSuffix(scanner.Text(), "\n"))
		if err != nil {
			log.Fatalf("provide some better input pls. Error = %s with num %d", err.Error(), num)
		}
		ints = append(ints, num)
	}

	return ints
}

func commaSplitFunc(data []byte, atEOF bool) (advance int, token []byte, err error) {
	if atEOF && len(data) == 0 {
		return 0, nil, nil
	}

	commaSearchBytes := []byte(",")

	if i := bytes.Index(data, commaSearchBytes); i >= 0 {
		return i + len(commaSearchBytes), data[0:i], nil
	}

	if atEOF {
		return len(data), data, nil
	}

	return 0, nil, nil
}
