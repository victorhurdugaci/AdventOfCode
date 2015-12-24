package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
)

type action func([]int)

const RequiredCapacity int = 150

func main() {
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    scanner := bufio.NewScanner(f)
    
    containers := []int{}
    for scanner.Scan() {
        container, ok := strconv.Atoi(scanner.Text())
        check(ok)
         
        containers = append(containers, container)
    }
    
    containerCount := len(containers)
    goodCombinations := 0
    
    minCombinationSize := 0
    minCombinationSizeCount := 0
    
    for i := 1; i < containerCount; i++ {
        forAllCombinations(containerCount, i, func(combination []int) {
            capacity := calculateCombinationCapacity(combination, containers)
            if (capacity == RequiredCapacity) {
                if (minCombinationSize == 0 || minCombinationSize == i) {
                    minCombinationSize = i
                    minCombinationSizeCount++
                }
                goodCombinations++
            }
        })
    }
    
    fmt.Printf("Good combinations: %d\n", goodCombinations);
    fmt.Printf("Good combinations min(%d): %d\n", minCombinationSize, minCombinationSizeCount);
}

func calculateCombinationCapacity(combination []int, containers []int) int {
    capacity := 0
    for i := 0; i < len(combination); i++ {
        capacity += containers[combination[i]]
    }
    return capacity
}

func forAllCombinations(n int, k int, act action) {
    if (k > n) {
        panic("k cannot be greater than n")
    }
    
    combination := make([]int, k)
    for i := 0; i < k; i++ {
        combination[i] = i
    }
    
    for {
        act(combination)
        ok := nextCombination(combination, n)
        if (!ok) {
            break
        }
    }
}

func nextCombination(combination []int, n int) bool {
    combinationLen := len(combination)
    lastPos := combinationLen - 1
    for {
        // First increment the first possible (starting from right)
        for lastPos >= 0 {
            if (combination[lastPos] < n - 1) {
                combination[lastPos]++
                break
            }
            lastPos--
        }
        if (lastPos == -1) {
            break
        }
        
        // Then try to fill the remaining spaces with the value previous+1
        var i int
        for i = lastPos + 1; i < combinationLen; i++ {
            if (combination[i-1] >= n-1) {
                break
            }
            combination[i] = combination[i-1] + 1
        }
        
        if (i == combinationLen) {
            return true
        }
    }
    
    return false
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}

