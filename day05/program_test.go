package main

import (
    "testing"
)

func Test_isNiceString(t *testing.T) {
    // Input too short
    isNiceStringTest(t, "", false)
    isNiceStringTest(t, "a", false)
    isNiceStringTest(t, "aa", false)
    isNiceStringTest(t, "ab", false)
    
    isNiceStringTest(t, "aaa", true)
    isNiceStringTest(t, "aaio", true)
    isNiceStringTest(t, "xaaio", true)
    
    // Has bad group
    isNiceStringTest(t, "aaaab", false)
    isNiceStringTest(t, "aaacd", false)
    isNiceStringTest(t, "aaapq", false)
    isNiceStringTest(t, "aaaxy", false)
    isNiceStringTest(t, "aaioxy", false)
    isNiceStringTest(t, "xaaiopq", false)
    
    isNiceStringTest(t, "abaaa", false)
    isNiceStringTest(t, "abaaio", false)
    isNiceStringTest(t, "abxaaio", false)
    
    isNiceStringTest(t, "aaba", false)
    isNiceStringTest(t, "aabio", false)
    isNiceStringTest(t, "xaabio", false)
    
    // From puzzle text
    isNiceStringTest(t, "ugknbfddgicrmopn", true)
    isNiceStringTest(t, "aaa", true)
    isNiceStringTest(t, "jchzalrnumimnmhp", false)
    isNiceStringTest(t, "haegwjzuvuyypxyu", false)
    isNiceStringTest(t, "dvszwmarrgswjxmb", false)
}

func Test_isNiceStringV2(t *testing.T) {
    // Input too short
    isNiceStringTest(t, "", false)
    isNiceStringTest(t, "a", false)
    isNiceStringTest(t, "aa", false)
    isNiceStringTest(t, "ab", false)
    isNiceStringTest(t, "xxx", false)
    isNiceStringTest(t, "xax", false)
    
    isNiceStringTestV2(t, "xxxx", true)
    isNiceStringTestV2(t, "xxaxx", true)
    
    isNiceStringTestV2(t, "xxabaxxab", true)
      
    isNiceStringTestV2(t, "zxgzx", false)
    isNiceStringTestV2(t, "zxgxz", false)
      
    // From puzzle text
    isNiceStringTestV2(t, "qjhvhtzxzqqjkmpb", true)
    isNiceStringTestV2(t, "xxyxx", true)
    isNiceStringTestV2(t, "uurcxstgmygtbstg", false)
    isNiceStringTestV2(t, "ieodomkazucvgmuy", false)
}

func isNiceStringTest(t *testing.T, input string, expectedNice bool) {
   actualNice := isNiceString(input)
   if (expectedNice != actualNice) {
       t.Error(input, ": expect ", expectedNice);
   }
}

func isNiceStringTestV2(t *testing.T, input string, expectedNice bool) {
   actualNice := isNiceStringV2(input)
   if (expectedNice != actualNice) {
       t.Error(input, ": expect ", expectedNice);
   }
}