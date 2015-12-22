package main

import (
    "fmt"
    "testing"
)


func Test_countCharactersEncodeTest(t *testing.T) {
    countCharactersEncodeTest(t, "\"\\xa5\"", 6, 11)
    countCharactersEncodeTest(t, "\"\\xz5\"", 6, 11)
    countCharactersEncodeTest(t, "\"\\\"", 3, 8)
    countCharactersEncodeTest(t, "\"\\z\"", 4, 9)
    countCharactersEncodeTest(t, "\"\\x5z\"", 6, 11)
    countCharactersEncodeTest(t, "\"\\\\\"", 4, 10)
    
    // From puzzle text
    countCharactersEncodeTest(t, "\"\"", 2, 6)
    countCharactersEncodeTest(t, "\"abc\"", 5, 9)
    countCharactersEncodeTest(t, "\"aaa\\\"aaa\"", 10, 16)
    countCharactersEncodeTest(t, "\"\\x27\"", 6, 11)
}

func Test_countCharactersDecodeTest(t *testing.T) {
    countCharactersDecodeTest(t, "\"\\xa5\"", 6, 1)
    countCharactersDecodeTest(t, "\"\\xz5\"", 6, 4)
    countCharactersDecodeTest(t, "\"\\\"", 3, 1)
    countCharactersDecodeTest(t, "\"\\z\"", 4, 2)
    countCharactersDecodeTest(t, "\"\\x5z\"", 6, 4)
    countCharactersDecodeTest(t, "\"\\\\\"", 4, 1)
    
    // From puzzle text
    countCharactersDecodeTest(t, "\"\"", 2, 0)
    countCharactersDecodeTest(t, "\"abc\"", 5, 3)
    countCharactersDecodeTest(t, "\"aaa\\\"aaa\"", 10, 7)
    countCharactersDecodeTest(t, "\"\\x27\"", 6, 1)
}

func Test_isHex(t *testing.T) {
    assertEqual(t, true, isHex('0'))
    assertEqual(t, true, isHex('1'))
    assertEqual(t, true, isHex('2'))
    assertEqual(t, true, isHex('3'))
    assertEqual(t, true, isHex('4'))
    assertEqual(t, true, isHex('5'))
    assertEqual(t, true, isHex('6'))
    assertEqual(t, true, isHex('7'))
    assertEqual(t, true, isHex('8'))
    assertEqual(t, true, isHex('9'))
    assertEqual(t, true, isHex('a'))
    assertEqual(t, true, isHex('b'))
    assertEqual(t, true, isHex('c'))
    assertEqual(t, true, isHex('d'))
    assertEqual(t, true, isHex('e'))
    assertEqual(t, true, isHex('f'))
    
    assertEqual(t, false, isHex('g'))
    assertEqual(t, false, isHex('-'))
    assertEqual(t, false, isHex('+'))
    assertEqual(t, false, isHex('.'))
}

func countCharactersDecodeTest(t *testing.T, input string, expectedCode int, expectedPrinted int) {
    actualCode, actualPrinted := countCharactersDecode(input)
    assertEqual(t, expectedCode, actualCode)
    assertEqual(t, expectedPrinted, actualPrinted)
}

func countCharactersEncodeTest(t *testing.T, input string, expectedCode int, expectedEncoded int) {
    actualCode, actualEncoded := countCharactersEncode(input)
    assertEqual(t, expectedCode, actualCode)
    assertEqual(t, expectedEncoded, actualEncoded)
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}