package main

import "fmt"

func countLettersInNumberWords(n int) int {
	numbersInWords := make(map[int]string)
	numbersInWords[1] = "one"
	numbersInWords[2] = "two"
	numbersInWords[3] = "three"
	numbersInWords[4] = "four"
	numbersInWords[5] = "five"
	numbersInWords[6] = "six"
	numbersInWords[7] = "seven"
	numbersInWords[8] = "eight"
	numbersInWords[9] = "nine"
	numbersInWords[10] = "ten"
	numbersInWords[11] = "eleven"
	numbersInWords[12] = "twelve"
	numbersInWords[13] = "thirteen"
	numbersInWords[14] = "fourteen"
	numbersInWords[15] = "fifteen"
	numbersInWords[16] = "sixteen"
	numbersInWords[17] = "seventeen"
	numbersInWords[18] = "eighteen"
	numbersInWords[19] = "nineteen"
	numbersInWords[20] = "twenty"
	numbersInWords[30] = "thirty"
	numbersInWords[40] = "forty"
	numbersInWords[50] = "fifty"
	numbersInWords[60] = "sixty"
	numbersInWords[70] = "seventy"
	numbersInWords[80] = "eighty"
	numbersInWords[90] = "ninety"
	numbersInWords[100] = "onehundred"
	numbersInWords[1000] = "onethousand"

	switch {
	case n < 20:
		return len(numbersInWords[n])
	case n < 100:
		firstDigit := n - (n % 10)
		lastDigit := n % 10
		if n%10 == 0 {
			return len(numbersInWords[n])
		}
		return len(numbersInWords[firstDigit] + numbersInWords[lastDigit])
	case n < 1000:
		firstDigit := n / 100
		last2Digits := n % 100
		if n%100 == 0 {
			return len(numbersInWords[firstDigit] + "hundred")
		}
		return len(numbersInWords[firstDigit]+"hundred"+"and") + countLettersInNumberWords(last2Digits)
	case n == 1000:
		return len(numbersInWords[n])
	}
	return 0
}

func main() {
	total := 0
	for i := 1; i <= 1000; i++ {
		total += countLettersInNumberWords(i)
	}
	fmt.Println(total)
}
