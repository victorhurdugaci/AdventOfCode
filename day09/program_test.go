package main

import (
    "fmt"
    "testing"
)


func Test_countCharactersEncodeTest(t *testing.T) {
    forAllPermutationsCountTest(t, 0, 1)
    forAllPermutationsCountTest(t, 1, 1)
    forAllPermutationsCountTest(t, 2, 2)
    forAllPermutationsCountTest(t, 3, 6)
    forAllPermutationsCountTest(t, 4, 24)
    forAllPermutationsCountTest(t, 5, 120)
    forAllPermutationsCountTest(t, 6, 720)
    forAllPermutationsCountTest(t, 7, 5040)
    forAllPermutationsCountTest(t, 8, 40320)
}

func forAllPermutationsCountTest(t *testing.T, itemCount int, expectedPermutations int) {
    count := 0
    
    forAllPermutations(itemCount, func(_ []int) {
        //fmt.Printf("%v\n", permutation)
        count++
    })
    
    assertEqual(t, expectedPermutations, count)
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}