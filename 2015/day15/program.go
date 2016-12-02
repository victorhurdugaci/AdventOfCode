package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

type ingredient struct{
    name string
    properties []int
}

const Spoons int = 100
const IngredientsCount int = 4
const PropertyCount int = 5
const TargetCalories int = 500

func main() {
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    scanner := bufio.NewScanner(f)

    var ingredients [IngredientsCount]ingredient

    i := 0
    for scanner.Scan() {
        ingredients[i] = parseIngredient(scanner.Text())
        i++
    }
    
    bestScore := 0
    bestScoreCaloryTarget := 0
    quantities := [] int {0, 0, 0, 100}
    for {
        cookieScore := evaluateRecipe(ingredients[:], quantities)
        calories := calculateCalories(ingredients[:], quantities)
        
        bestScore = max(bestScore, cookieScore) 
        if (calories == TargetCalories) {
            bestScoreCaloryTarget = max(bestScoreCaloryTarget, cookieScore)
        }
        
        if (!incrementQuantities(quantities)) {
            break
        }
    }
    
    fmt.Printf("Best score: %d\n", bestScore)
    fmt.Printf("Best score calories: %d\n", bestScoreCaloryTarget)
}

func incrementQuantities(quantities []int) bool {
    lastIndex := len(quantities) - 1
    
    remaining := quantities[lastIndex]
    quantities[lastIndex] = 0
    
    var i int
    for i = lastIndex - 1; i >= 0; i-- {
        if (remaining > 0) {
            quantities[i]++
            remaining--
            break
        } else {
            remaining += quantities[i]
            quantities[i] = 0
        }
    }
    
    if (i == -1) {
        return false
    }
    
    quantities[lastIndex] = remaining
    return true
}

func evaluateRecipe(ingredients []ingredient, quantities []int) int {
    recipeScore := 1
    for p := 0; p < PropertyCount-1; p++ {
        propSum := 0
        for q := 0; q < len(quantities); q++ {
            propSum += ingredients[q].properties[p] * quantities[q]
        }
        
        recipeScore *= max(0, propSum)
    } 
    
    return recipeScore
}

func calculateCalories(ingredients []ingredient, quantities []int) int {
    calories := 0
    for q := 0; q < len(quantities); q++ {
        calories += ingredients[q].properties[PropertyCount-1] * quantities[q]
    }
    
    return calories
}

func parseIngredient(line string) ingredient {
    tokens := strings.Split(line, " ")
    
    
    properties := make([]int, 5)
    
    for i := 0; i < 5; i++ {
        p, err := strconv.Atoi(strings.TrimRight(tokens[2 * (i+1)], ","))
        check(err)
        
        properties[i] = p 
    }
    
    return ingredient {
        name: strings.Trim(tokens[0], ":"),
        properties: properties,
    }
}

func max(a int, b int) int {
    if (a > b) {
        return a
    }
    
    return b
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}

