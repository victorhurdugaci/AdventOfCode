package main

import (
    "fmt"
    "testing"
)

func Test_calculateCombinationCapacity(t *testing.T) {
    containers := []int { 20, 15, 10, 5, 5 }
    calculateCombinationCapacityTest(t, []int{ 0, 1 }, containers, 35)
    calculateCombinationCapacityTest(t, []int{ 1, 3, 4 }, containers, 25)
    calculateCombinationCapacityTest(t, []int{ 2, 4 }, containers, 15)
}

func Test_combinationCount(t *testing.T) {
    combinationCountTest(t, 5, 1, 5)
    combinationCountTest(t, 5, 2, 10)
    combinationCountTest(t, 5, 3, 10)
    combinationCountTest(t, 5, 4, 5)
    combinationCountTest(t, 5, 5, 1)
}

func Test_nextCombination(t *testing.T) {
    nextCombinationTest(t, []int { 0, 1, 2, 3 }, 5, true, []int { 0, 1, 2, 4})
    nextCombinationTest(t, []int { 0, 1, 2, 4 }, 5, true, []int { 0, 1, 3, 4})
    nextCombinationTest(t, []int { 0, 1, 3, 4 }, 5, true, []int { 0, 2, 3, 4})
    nextCombinationTest(t, []int { 3 }, 6, true, []int { 4})
    nextCombinationTest(t, []int { 5 }, 6, false, nil)
    nextCombinationTest(t, []int { 1, 2, 3, 4 }, 5, false, nil)
}

func calculateCombinationCapacityTest(t *testing.T, combination []int, containers []int, expectedCapacity int) {
    actualCapacity := calculateCombinationCapacity(combination, containers)
    assertEqual(t, expectedCapacity, actualCapacity)
}

func combinationCountTest(t *testing.T, n int, k int, expectedCount int) {
    actualCount := 0
    forAllCombinations(n, k, func(_ []int) {
        actualCount++
    })
    
    assertEqual(t, expectedCount, actualCount)
}

func nextCombinationTest(t *testing.T, input []int, n int, expectedOk bool, expectedCombination []int) {
    actualOk := nextCombination(input, n)
    assertEqual(t, expectedOk, actualOk)
    if (expectedOk) {
        for i := 0; i < len(expectedCombination); i++ {
            assertEqual(t, expectedCombination[i], input[i])
        }
    }
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}