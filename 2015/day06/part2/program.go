package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

type lightOperation func(*int)

type action struct {
    operation lightOperation
    startX int
    startY int
    endX int
    endY int
}

func (a action) applyTo(grid [][]int) {
    for x := a.startX; x <= a.endX; x++ {
        for y := a.startY; y <= a.endY; y++ {
            a.operation(&grid[x][y])
        }
    }
}

func main() {
    f, err := os.Open("../input")
    check(err) 
    defer f.Close()
    
    scanner := bufio.NewScanner(f)
    
    grid := createGrid(1000, 1000)
    
    for scanner.Scan() {
        act := parseAction(scanner.Text())
        act.applyTo(grid)
    }
    
    litBrightness := 0
    for y := 0; y < 1000; y++ {
        for x := 0; x < 1000; x++ {
            litBrightness += grid[x][y]
        }
    }
    
    fmt.Printf("Lit brightness: %d\n", litBrightness)
}

func parseAction(line string) action {
    split := strings.Split(line, " ")
    
    op := op_toggle
    startIndex := 1
    endIndex := 3
    
    if (len(split) != 4) {
        if (split[1] == "on") {
            op = op_turnon
        } else {
            op = op_turnoff
        }
        
        startIndex = 2
        endIndex = 4
    }
    
    startX, startY := parseCoordinates(split[startIndex])
    endX, endY := parseCoordinates(split[endIndex])
    
    // Make sure the points are normalized in space
    arrangeAscending(&startX, &endX)
    arrangeAscending(&startY, &endY)
    
    return action {
        operation: op,
        startX: startX,
        endX: endX,
        startY: startY,
        endY: endY,
    }
}

func parseCoordinates(input string) (int, int) {
    coordinates := strings.Split(input, ",")
    
    x, err := strconv.Atoi(coordinates[0])
    check(err) 
    
    y, err := strconv.Atoi(coordinates[1])
    check(err) 
    
    return x, y
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}

func op_toggle(light *int) {
    op_turnon(light)
    op_turnon(light)
}

func op_turnon(light *int) {
    *light++
}

func op_turnoff(light *int) {
    if (*light > 0) {
        *light--
    }
}

func arrangeAscending(a *int, b *int) {
    if (*a > *b) {
        swap := *b
        *b = *a
        *a = swap
    }
}

func createGrid(w int, h int) [][]int {
    grid := make([][]int, h)
    for x := 0; x < h; x++ {
        grid[x] = make([]int, w)
    }
    return grid
}

