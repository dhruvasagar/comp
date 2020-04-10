package main

import (
	"fmt"
	"time"
)

func main() {
	startDate := time.Date(1901, 1, 1, 0, 0, 0, 0, time.UTC)
	endDate := time.Date(2000, 12, 31, 0, 0, 0, 0, time.UTC)
	cnt := 0
	for t := startDate; t.Before(endDate); t = t.AddDate(0, 1, 0) {
		if t.Weekday() == time.Sunday {
			cnt++
		}
	}
	fmt.Println(cnt)
}
