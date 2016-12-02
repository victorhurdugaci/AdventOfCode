package main

import (
    "fmt"
    "testing"
)

func Test_incrementString(t *testing.T) {
    incrementStringTest(t, "a", true, "b")
    incrementStringTest(t, "y", true, "z")
    incrementStringTest(t, "z", false, "")
    incrementStringTest(t, "aa", true, "ab")
    incrementStringTest(t, "zz", false, "")
    incrementStringTest(t, "az", true, "ba")
    incrementStringTest(t, "abczzz", true, "abdaaa")
    incrementStringTest(t, "zzzzz", false, "")
}

func Test_isValidPassword(t *testing.T) {
    isValidPasswordTest(t, "abcffaa", false) // too short
    isValidPasswordTest(t, "ghjaabaa", false)
    
    isValidPasswordTest(t, "ibcdffaa", false)
    isValidPasswordTest(t, "obcdffaa", false)
    isValidPasswordTest(t, "lbcdffaa", false)
    
    // From puzzle text
    isValidPasswordTest(t, "hijklmmn", false)
    isValidPasswordTest(t, "abbceffg", false)
    isValidPasswordTest(t, "abbcegjk", false)
    isValidPasswordTest(t, "abcdffaa", true)
    isValidPasswordTest(t, "ghjaabcc", true)
}

func Test_findNextPassword(t *testing.T) {
    findNextPasswordTest(t, "zzzzzzzz", false, "")
    findNextPasswordTest(t, "zzzzzzza", false, "")
        
    // From puzzle text
    findNextPasswordTest(t, "abcdefgh", true, "abcdffaa")
    findNextPasswordTest(t, "ghijklmn", true, "ghjaabcc")
}

func findNextPasswordTest(t *testing.T, input string, expectedFound bool, expectedString string) {
    actualString, actualFound := findNextPassword(input)
    assertEqual(t, expectedFound, actualFound)
    if (expectedFound) {
        assertEqual(t, expectedString, actualString)
    }
}

func isValidPasswordTest(t *testing.T, input string, expectedValid bool) {
    actualValid := isValidPassword(input)
    assertEqual(t, expectedValid, actualValid)
}

func incrementStringTest(t *testing.T, input string, expectedFound bool, expectedString string) {
    actualString, actualFound := incrementString(input)
    assertEqual(t, expectedFound, actualFound)
    if (expectedFound) {
        assertEqual(t, expectedString, actualString)
    }
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}