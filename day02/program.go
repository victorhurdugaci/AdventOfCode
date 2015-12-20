package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func main() {
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    
    paperNeeded := 0
    ribbonNeeded := 0
    
    scanner := bufio.NewScanner(f)
    for scanner.Scan() {
        l, w, h := parsePackageDimensions(scanner.Text())
        paperNeeded += calculatePaperNeeded(l, w, h)
        ribbonNeeded += calculateRibbonNeeded(l, w, h)
    }
    
    fmt.Printf("Paper: %d\n", paperNeeded)
    fmt.Printf("Ribbon: %d\n", ribbonNeeded)
}

func calculateRibbonNeeded(l int, w int, h int) int {
    perimeter1 := 2 * (l + w)
    perimeter2 := 2 * (w + h)
    perimeter3 := 2 * (h + l)
    
    minPermimeter := min(perimeter1, min(perimeter2, perimeter3))
    
    volume := l * w * h
    
    return minPermimeter + volume
}

func calculatePaperNeeded(l int, w int, h int) int {
    side1 := l * w
    side2 := w * h
    side3 := h * l
    
    area := 2 * (side1 + side2 + side3)
    minSide := min(side1, min(side2, side3))
    
    return  area + minSide
}

func parsePackageDimensions(line string) (int, int, int) {
    dim := strings.Split(line, "x")
    l, err := strconv.Atoi(dim[0])
    check(err)
    
    w, err := strconv.Atoi(dim[1])
    check(err)
    
    h, err := strconv.Atoi(dim[2])
    check(err)
    
    return l, w, h
}

func min(a int, b int) int {
    if (a > b) {
        return b
    }
    
    return a
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}

