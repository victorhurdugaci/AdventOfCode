package main

import (
    "fmt"
    "os"
)

const input int = 33100000
var part2 bool = false

func main() {
    if (len(os.Args) == 2 &&
         os.Args[1] == "part2") {
         part2 = true
    }
  
    // No point in starting at 0 because 
    // the numbers before 8000 summed up don't make the input
    // 8000 x 8001 / 2 = 32 004 000
    i := 8000
    for {        
        sum := sumOfDivisors(i)
        if (i % 1000 == 0) {
            fmt.Printf("Now at %d. Sum: %d\n", i, sum)
        }
        if (sum >= input) {
            fmt.Printf("Found it: %d\n", i)
            break
        }
        
        i++
    }
}

// Works for n > 1
func sumOfDivisors(n int) int {
    sum := n
    stopSearch := int(n/2)
    
    i := 1
    if (part2) {
        i = int(n/50)
    }
    
    for ; i <= stopSearch; i++ {
        if (n % i == 0) {
            if (part2 && n / i > 50) {
                continue
            }
            sum += i
        }
    }
    
    if (part2) {
        sum = sum * 11
    } else {
        sum = sum * 10
    }
    return sum
}

func sumOfDivisorsOnly50(n int) int {
    sum := n
    stopSearch := int(n/2)
    
    for i := 1; i <= stopSearch; i++ {
        if (n % i == 0 && n / i >= 50) {
            sum += i
        }
    }
    
    return sum * 10
}