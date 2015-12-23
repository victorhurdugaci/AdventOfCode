package main

import (
    "bufio"
    "fmt"
    "math"
    "os"
    "strconv"
    "strings"
)

type action func([]int)

var cityMapping map[string]int 
var distances [][]int

func main() {
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    scanner := bufio.NewScanner(f)
    
    cityMapping = make(map[string]int)
    distances = make2DMatrix(8, 8)
    
    for scanner.Scan() {
        city1, city2, distance := parseDistance(scanner.Text())
        addDistance(city1, city2, distance)
    }
    
    minDistance := math.MaxInt32
    minPermutation := make([]int, len(distances))
    
    maxDistance := 0
    maxPermutation := make([]int, len(distances))
    
    forAllPermutations(len(distances), func(permutation []int) {
        // Compute the total distance
        candidateDistance := 0
        for i := 0; i < len(permutation) - 1; i++ {
            candidateDistance += distances[permutation[i]][permutation[i+1]]
        }
        
        if (minDistance > candidateDistance) {
            minDistance = candidateDistance
            copy(minPermutation, permutation)
        }
        
        if (maxDistance < candidateDistance) {
            maxDistance = candidateDistance
            copy(maxPermutation, permutation)
        }
    })
    
    fmt.Printf("Min Route (%d): %v\n", minDistance, minPermutation);
    fmt.Printf("Max Route (%d): %v\n", maxDistance, maxPermutation);
}

func addDistance(city1 string, city2 string, distance int) {
    city1Idx, found := cityMapping[city1]
    if (!found) {
        city1Idx = len(cityMapping)
        cityMapping[city1] = city1Idx
    }
    
    city2Idx, found := cityMapping[city2]
    if (!found) {
        city2Idx = len(cityMapping)
        cityMapping[city2] = city2Idx
    }
    
    distances[city1Idx][city2Idx] = distance
    distances[city2Idx][city1Idx] = distance
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

func parseDistance(line string) (string, string, int) {
    tokens := strings.Split(line, " ")
    
    distance, err := strconv.Atoi(tokens[4])
    check(err)
    
    return strings.TrimSpace(tokens[0]), strings.TrimSpace(tokens[2]), distance
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

