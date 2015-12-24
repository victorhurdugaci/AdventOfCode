package main

import (
    "bufio"
    "fmt"
    "os"
)

type action func(y int, x int)

func main() {
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    scanner := bufio.NewScanner(f)
    
    matrix := [][]int{}
    for scanner.Scan() {
        line := parseMatrixLine(scanner.Text())
        matrix = append(matrix, line)
    }
    
    state1 := matrix
    state2 := copyMatrix(matrix)
    
    h := len(matrix)
    w := len(matrix[0])
    
    state2[0][0] = 1
    state2[h-1][0] = 1
    state2[0][w-1] = 1
    state2[h-1][w-1] = 1
    
    for i := 0; i < 100; i++ {
        state1 = computeNextState(state1, false)
        state2 = computeNextState(state2, true)
    }
    onCount1 := 0
    onCount2 := 0
    forEach2DMatrixElement(state1, func(y int, x int) {
        onCount1 += state1[y][x]
        onCount2 += state2[y][x]
    })
    
    fmt.Printf("On count 1: %d\n", onCount1)
    fmt.Printf("On count 2: %d\n", onCount2)
}

func computeNextState(start [][]int, stuckCorners bool) [][]int {
    h := len(start)
    w := len(start[0])
    
    nextState := make2DMatrix(w, h)
    
    forEach2DMatrixElement(start, func(y int, x int) {
        if (stuckCorners && 
            ((y == 0 && x == 0) ||
             (y == 0 && x == w-1) ||
             (y == h-1 && x == 0) ||
             (y == h-1 && x == w-1))) {
            nextState[y][x] = 1
            return
        }
        neighborCount := getOnNeightborCount(start, y, x)
        if ((start[y][x] == 1 && (neighborCount == 2 || neighborCount == 3)) ||
            (start[y][x] == 0 && neighborCount == 3)) {
            nextState[y][x] = 1
        }
    })
    
    return nextState
}

func parseMatrixLine(line string) []int {
    result := make([]int, len(line))
    for i := 0; i < len(line); i++ {
        if (line[i] == '#') {
            result[i] = 1
        } else {
            result[i] = 0
        }
    }
    return result;
}

func getOnNeightborCount(matrix [][]int, y int, x int) int {
    // 0 1 2
    // 3 x 5
    // 6 7 8
    onNeighborCount := getMatrixElementSafe(matrix, y-1, x-1) // 0
    onNeighborCount += getMatrixElementSafe(matrix, y-1, x) // 1
    onNeighborCount += getMatrixElementSafe(matrix, y-1, x+1) // 2
    onNeighborCount += getMatrixElementSafe(matrix, y, x-1) // 3
    onNeighborCount += getMatrixElementSafe(matrix, y, x+1) // 5
    onNeighborCount += getMatrixElementSafe(matrix, y+1, x-1) // 6
    onNeighborCount += getMatrixElementSafe(matrix, y+1, x) // 7
    onNeighborCount += getMatrixElementSafe(matrix, y+1, x+1) // 8
    
    return onNeighborCount
}

func getMatrixElementSafe(matrix [][]int, y int, x int) int {
    if (x < 0 || len(matrix[0]) <= x ||
        y < 0 || len(matrix) <= y) {
            return 0
    }
    
    return matrix[y][x]
}

func forEach2DMatrixElement(matrix [][]int, act action) {
    h := len(matrix)
    w := len(matrix[0])
    
    for y := 0; y < h; y++ {
        for x := 0; x < w; x++ {
            act(y, x)
        }
    }
}

func copyMatrix(src [][]int) [][]int {
    h := len(src)
    w := len(src[0])
    
    dst := make([][]int, h)
    for x := 0; x < h; x++ {
        dst[x] = make([]int, w)
        copy(dst[x], src[x])
    }
    return dst
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


