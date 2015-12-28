package main

import (
    "bufio"
    "fmt"
    "math"
    "os"
    "strconv"
)

var input []uint64

func main() {
    var minQE uint64 = math.MaxUint64
    var minQEGroupLength = 0
    
    parseInput()
    var inputLen uint64 = uint64(len(input))
     
    // Since the groups must be equal, compute the expected weight here
    var i, expectedWeight uint64
    for i = 0; i < inputLen; i++ {
        expectedWeight += input[i]
    }
    expectedWeight = expectedWeight / 3
    
    // There's no point in looking for groups that cannot sum up to that size
    var minGroupSize, sum uint64
    for i = inputLen - 1; i >= 0; i-- {
        sum += input[i]
        minGroupSize++
        if (sum >= expectedWeight) {
            break
        }
    }
   
    forSumGroup(input, expectedWeight, minGroupSize, func(group1 []uint64, remaining1 []uint64) bool {
        g1Len := len(group1)
        if (minQEGroupLength != 0 && minQEGroupLength < g1Len) {
            return false
        }
        
        var qe uint64 = 1
        for i := 0; i<g1Len; i++ {
            qe *= group1[i]
        }
        
        fmt.Printf("%v %v\n", group1, remaining1)
        
        forSumGroup(remaining1, expectedWeight, minGroupSize, func(_ []uint64, _ []uint64) bool {
            // if we get here it means we found a group
            if (qe < minQE) {
                minQE = qe
                minQEGroupLength = g1Len
            }
            return false
        })
        
        return true
    })

    fmt.Printf("Min QE: %v\n", minQE)
}

type action func(group []uint64, remaining []uint64) bool

func forSumGroup(input []uint64, expectedSum uint64, minGroupSize uint64, act action) {
    var inputLen uint64 = uint64(len(input))
    var possibleCombinations uint64 = uint64(math.Pow(2, float64(inputLen)))
    
    var gSize uint64
    more := true
    for gSize = minGroupSize; gSize <= inputLen && more; gSize++ {
        var remaining = make([]uint64, inputLen-gSize)
        var group = make([]uint64, gSize)
        
        var g uint64
        for g = 0; g < possibleCombinations && more; g++ {
            gOnes := popcount(uint64(g))
            if (gOnes != gSize) {
                continue
            }
            
            var s uint64
            var i uint64
            for i = 0; gOnes > 0 && more; i++ {
                var mask uint64 = 1 << i
                if (g & mask == mask) {
                    s += input[i] 
                    gOnes-- 
                }
            }
                
            if (s != expectedSum) {
                continue
            }
    
            ri := 0
            gi := 0
            for i = 0; i < inputLen; i++ {
                var mask uint64 = 1 << i
                if (g & mask != mask) {
                    remaining[ri] = input[i]
                    ri++
                } else {
                    group[gi] = input[i]
                    gi++
                }
            }
                
            more = act(group, remaining)
        }
    }
}

func popcount(x uint64) uint64 {
    const (
        m1  = 0x5555555555555555 //binary: 0101...
        m2  = 0x3333333333333333 //binary: 00110011..
        m4  = 0x0f0f0f0f0f0f0f0f //binary:  4 zeros,  4 ones ...
        h01 = 0x0101010101010101 //the sum of 256 to the power of 0,1,2,3...
    )
    x -= (x >> 1) & m1             //put count of each 2 bits into those 2 bits
    x = (x & m2) + ((x >> 2) & m2) //put count of each 4 bits into those 4 bits
    x = (x + (x >> 4)) & m4        //put count of each 8 bits into those 8 bits
    return (x * h01) >> 56  //returns left 8 bits of x + (x<<8) + (x<<16) + (x<<24) + ...
}

func parseInput() {
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    scanner := bufio.NewScanner(f)
    
    parseInputInternal(scanner, 0)
}

func parseInputInternal(scanner *bufio.Scanner, count int) {
    if (!scanner.Scan()) {
        input = make([]uint64, count)
        return
    }
    
    text := scanner.Text()
    parseInputInternal(scanner, count+1)
    
    num, err := strconv.Atoi(text)
    check(err)
    
    input[count] = uint64(num)
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}


