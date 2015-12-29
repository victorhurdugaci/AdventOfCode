package main

import (
    "fmt"
)

const row uint64 = 2978
const col uint64 = 3083
const startCode uint64 = 20151125 

const multiplier uint64 = 252533
const divider uint64 = 33554393

func main() {
    var codeIndex uint64 = calculateCodeIndex(row, col) 
    var code uint64 = calculateNthCode(codeIndex) 
    fmt.Printf("Code: %v\n",code)
}

func calculateNthCode(n uint64) uint64 {
    var code uint64 = startCode
    
    for n > 1 {
        code *= multiplier
        code = code % divider
        n--
    }
    
    return code
}

// Calculates the index of a code
func calculateCodeIndex(row uint64, col uint64) uint64 {
    // The table is a triangle and the element 
    // we're looking for in on the hypotenuse of an 
    // isosceles right angle triangle
    
    // First figure the length of the side
    var side uint64 = row + col - 1

    // Calculate how many numbers are in that triangle
    var totalNumbers uint64 = side * (side + 1) / 2
    
    // Now get the index
    var index = totalNumbers - row + 1
    
    return index
}