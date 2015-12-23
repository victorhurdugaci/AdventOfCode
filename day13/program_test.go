package main

import (
    "fmt"
    "testing"
)

func Test_parseGuestList(t *testing.T) {
    parseGuestListTest(t, 
        "Alice would gain 26 happiness units by sitting next to Carol.",
        "Alice", "Carol", 26);
        
    parseGuestListTest(t, 
        "Alice would lose 82 happiness units by sitting next to David.",
        "Alice", "David", -82);
}

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
        count++
    })
    
    assertEqual(t, expectedPermutations, count)
}

func parseGuestListTest(t *testing.T, input string, expectedGuest1 string, expectedGuest2 string, expectedHappiness int) {
    actualGuest1, actualGuest2, actualHappiness := parseGuestList(input)
    assertEqual(t, expectedGuest1, actualGuest1)
    assertEqual(t, expectedGuest2, actualGuest2)
    assertEqual(t, expectedHappiness, actualHappiness)
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}