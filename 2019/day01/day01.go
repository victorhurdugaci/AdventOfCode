package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
)

func main() {
	file, err := os.Open("input")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	sum1 := 0
	sum2 := 0
	for scanner.Scan() {
		s := scanner.Text()
		mass, _ := strconv.Atoi(s)
		sum1 += fuel(mass)
		sum2 += fuel2(mass)
	}

	fmt.Println(sum1)
	fmt.Println(sum2)
}

func fuel(mass int) int {
	massF := float64(mass)
	return int(math.Floor(massF/3) - 2)
}

func fuel2(mass int) int {
	f := fuel(mass)
	if f <= 0 {
		return 0
	}

	return f + fuel2(f)
}
