package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

type action func([]int)

var nameMapping map[string]int 
var preferences [][]int

var totalGuests int = 8

func main() {
    includeSelf := false
    if (len(os.Args) == 2 &&
        os.Args[1] == "part2") {
        includeSelf = true
        totalGuests += 1
    }
    
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    scanner := bufio.NewScanner(f)
    
    nameMapping = make(map[string]int)
    preferences = make2DMatrix(totalGuests, totalGuests)
    
    for scanner.Scan() {
        guest1, guest2, hapiness := parseGuestList(scanner.Text())
        addHapiness(guest1, guest2, hapiness)
    }
    
    if (includeSelf) {
        nameMapping["self"] = len(nameMapping)
    }
    
    maxHapinesss := 0
    maxPermutation := make([]int, totalGuests)
    
    forAllPermutations(totalGuests, func(permutation []int) {
        
        // Compute the total hapiness
        candidateHapiness := 0
        
        for i := 0; i < totalGuests; i++ {
            leftIdx := i - 1
            if (leftIdx < 0) {
                leftIdx = totalGuests - 1
            }
            rightIdx := (i+1) % totalGuests
            
            candidateHapiness += preferences[permutation[i]][permutation[leftIdx]] +
                                 preferences[permutation[i]][permutation[rightIdx]]
        }
        
        if (maxHapinesss < candidateHapiness) {
            maxHapinesss = candidateHapiness
            copy(maxPermutation, permutation)
        }
    })
    
    fmt.Printf("Max hapiness (%d): %v\n", maxHapinesss, maxPermutation);
}

func addHapiness(guest1 string, guest2 string, hapiness int) {
    guest1Idx, found := nameMapping[guest1]
    if (!found) {
        guest1Idx = len(nameMapping)
        nameMapping[guest1] = guest1Idx
    }
    
    guest2Idx, found := nameMapping[guest2]
    if (!found) {
        guest2Idx = len(nameMapping)
        nameMapping[guest2] = guest2Idx
    }
    
    preferences[guest1Idx][guest2Idx] = hapiness 
}

func forAllPermutations(count int, act action) {
    used := make([]bool, count)
    permutation := make([]int, count)
    for i := 0; i < count; i++ {
        permutation[i] = -1
    }
    
    permute(permutation, used, 0, act)
}

func permute(permutation []int, used []bool, currentIdx int, act action) {
    if (currentIdx == len(permutation)) {
        act(permutation)
        return
    }
    
    for {
        next := getNextUnused(used, permutation[currentIdx])
        if (next == -1) {
            used[permutation[currentIdx]] = false
            permutation[currentIdx] = -1
            return
        }
        
        if (permutation[currentIdx] != -1) {
            used[permutation[currentIdx]] = false
        }
        used[next] = true
        permutation[currentIdx] = next
        
        permute(permutation, used, currentIdx + 1, act)
    }
}

func getNextUnused(used [] bool, start int) int {
    for i := start + 1; i< len(used); i++ {
        if (!used[i]) {
            return i
        }
    }
    
    return -1
}

func parseGuestList(line string) (string, string, int) {
    line = strings.Trim(line, ".")
    tokens := strings.Split(line, " ")
    
    hapiness, err := strconv.Atoi(tokens[3])
    check(err)
    
    if (tokens[2] == "lose") {
        hapiness = -hapiness
    }
    
    guest1 := strings.TrimSpace(tokens[0])
    guest2 := strings.TrimSpace(tokens[10])
    
    return guest1, guest2, hapiness
}

func max(a int, b int) int {
    if (a > b) {
        return a
    }    
    return b
}

func make2DMatrix(w int, h int) [][]int {
    matrix := make([][]int, h)
    for x := 0; x < h; x++ {
        matrix[x] = make([]int, w)
    }
    return matrix
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}

