package main

import (
    "fmt"
    "testing"
)

func Test_incrementQuantities(t *testing.T) {
    incrementQuantitiesTest(t, []int {0, 0, 100}, true, []int{0, 1, 99})
    incrementQuantitiesTest(t, []int {0, 100, 0}, true, []int{1, 0, 99})
    incrementQuantitiesTest(t, []int {10, 60, 30}, true, []int{10, 61, 29})
    incrementQuantitiesTest(t, []int {99, 1, 0}, true, []int{100, 0, 0})
    
    incrementQuantitiesTest(t, []int {100, 0, 0}, false, nil)
}

func Test_calculateCalories(t *testing.T) {
    ingredients := []ingredient {
        ingredient {
            name: "Butterscotch",
            properties: []int {-1, -2, 6, 3, 8},
        },
        ingredient {
            name: "Cinnamon",
            properties: []int {2, 3, -2, -1, 3},
        },
    }
    
    calculateCaloriesTest(t, ingredients, []int { 44, 56}, 520)
    calculateCaloriesTest(t, ingredients, []int { 66, 34}, 630)
}

func Test_evaluateRecipe(t *testing.T) {
    ingredients := []ingredient {
        ingredient {
            name: "Butterscotch",
            properties: []int {-1, -2, 6, 3, 8},
        },
        ingredient {
            name: "Cinnamon",
            properties: []int {2, 3, -2, -1, 3},
        },
    }
    
    evaluateRecipeTest(t, ingredients, []int { 44, 56}, 62842880)
    evaluateRecipeTest(t, ingredients, []int { 66, 34}, 0)
}

func Test_parseIngredient(t *testing.T) {
    parseIngredientTest(t, 
        "Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5",
        "Frosting",
        []int {4, -2, 0, 0, 5})
        
    parseIngredientTest(t, 
        "Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8",
        "Candy",
        []int {0, 5, -1, 0, 8})
}

func incrementQuantitiesTest(t *testing.T, input []int, expectedToIncrement bool, expectedResult []int) {
    ok := incrementQuantities(input)
    assertEqual(t, expectedToIncrement, ok)
    if (expectedToIncrement) {
        for i := 0; i < len(expectedResult); i++ {
            assertEqual(t, input[i], expectedResult[i])
        }
    }
}

func parseIngredientTest(t *testing.T, input string, expectedName string, expectedProperties []int) {
    ing := parseIngredient(input)
    assertEqual(t, expectedName, ing.name)
    for i := 0; i < len(expectedProperties); i++ {
        assertEqual(t, expectedProperties[i], ing.properties[i])    
    } 
}

func calculateCaloriesTest(t *testing.T, ingredients []ingredient, quantities []int, expectedCalories int) {
    actualCalories := calculateCalories(ingredients, quantities)
    assertEqual(t, expectedCalories, actualCalories)
}

func evaluateRecipeTest(t *testing.T, ingredients []ingredient, quantities []int, expectedScore int) {
    actualScore := evaluateRecipe(ingredients, quantities)
    assertEqual(t, expectedScore, actualScore)
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}