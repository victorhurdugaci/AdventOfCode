package main

import (
	"fmt"
	"io/ioutil"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	dat, err := ioutil.ReadFile("input")
	check(err)

	datLen := len(dat)
	halfDatLen := datLen / 2

	sum1 := 0
	sum2 := 0
	for i := 0; i < datLen; i++ {
		cur := dat[i]
		if cur == dat[(i+1)%datLen] {
			sum1 += int(cur - '0')
		}
		if cur == dat[(i+halfDatLen)%datLen] {
			sum2 += int(cur - '0')
		}
	}

	fmt.Printf("Part 1: %d\n", sum1)
	fmt.Printf("Part 2: %d\n", sum2)
}
