package main

import (
    "strconv"
    "testing"
)

func Test_applyTo_turnon(t *testing.T) {
    act := parseAction("turn on 2,3 through 4,4")
    input := createGrid(6, 6)
    expected := [][]int {
        {0,0,0,0,0,0},
        {0,0,0,0,0,0},
        {0,0,0,1,1,0},
        {0,0,0,1,1,0},
        {0,0,0,1,1,0},
        {0,0,0,0,0,0},
    }
    
    applyToTest(t, act, input, expected)
}

func Test_applyTo_turnoff(t *testing.T) {
    act := parseAction("turn off 2,3 through 4,4")
    input := [][]int {
        {1,1,1,1,1,1},
        {1,1,1,1,1,1},
        {1,1,1,1,0,1},
        {1,1,1,1,1,1},
        {1,1,1,1,0,1},
        {1,1,1,1,1,1},
    }
    expected := [][]int {
        {1,1,1,1,1,1},
        {1,1,1,1,1,1},
        {1,1,1,0,0,1},
        {1,1,1,0,0,1},
        {1,1,1,0,0,1},
        {1,1,1,1,1,1},
    }
    
    applyToTest(t, act, input, expected)
}

func Test_applyTo_toggle(t *testing.T) {
    act := parseAction("toggle 2,3 through 4,4")
    input := [][]int {
        {0,0,0,0,0,0},
        {0,0,0,0,0,0},
        {0,0,0,1,0,0},
        {0,0,0,0,1,0},
        {0,0,0,1,0,0},
        {0,0,0,0,0,0},
    }
    expected := [][]int {
        {0,0,0,0,0,0},
        {0,0,0,0,0,0},
        {0,0,0,3,2,0},
        {0,0,0,2,3,0},
        {0,0,0,3,2,0},
        {0,0,0,0,0,0},
    }
    
    applyToTest(t, act, input, expected)
}

func Test_applyTo_toggle_corners(t *testing.T) {
    act := parseAction("toggle 0,0 through 5,5")
    input := [][]int {
        {1,0,0,0,0,1},
        {0,0,0,0,0,0},
        {0,0,0,0,0,0},
        {0,0,0,0,0,0},
        {0,0,0,0,0,0},
        {1,0,0,0,0,1},
    }
    expected := [][]int {
        {3,2,2,2,2,3},
        {2,2,2,2,2,2},
        {2,2,2,2,2,2},
        {2,2,2,2,2,2},
        {2,2,2,2,2,2},
        {3,2,2,2,2,3},
    }
    
    applyToTest(t, act, input, expected)
}

func Test_applyTo_singlerow(t *testing.T) {
    act := parseAction("turn on 2,0 through 2,3")
    input := createGrid(6, 6)
    expected := [][]int {
        {0,0,0,0,0,0},
        {0,0,0,0,0,0},
        {1,1,1,1,0,0},
        {0,0,0,0,0,0},
        {0,0,0,0,0,0},
        {0,0,0,0,0,0},
    }
    
    applyToTest(t, act, input, expected)
}

func Test_applyTo_singlecolumn(t *testing.T) {
    act := parseAction("turn on 0,2 through 3,2")
    input := createGrid(6, 6)
    expected := [][]int {
        {0,0,1,0,0,0},
        {0,0,1,0,0,0},
        {0,0,1,0,0,0},
        {0,0,1,0,0,0},
        {0,0,0,0,0,0},
        {0,0,0,0,0,0},
    }
    
    applyToTest(t, act, input, expected)
}

func Test_parsePackageDimensions(t *testing.T) {
    parseActionTest(t,
        "turn off 660,55 through 986,197",
        660, 55,
        986, 197,
        0, 0)
        
    parseActionTest(t,
        "turn off 986,197 through 660,55",
        660, 55,
        986, 197,
        0, 0)
        
    parseActionTest(t,
        "turn on 240,129 through 703,297",
        240, 129,
        703, 297,
        1, 2)
    
    parseActionTest(t,
        "toggle 537,781 through 687,941",
        537, 781,
        687, 941,
        2, 3)
}

func parseActionTest(
    t *testing.T, 
    input string, 
    expectedStartX int, 
    expectedStartY int,
    expectedEndX int,
    expectedEndY int,
    opResult0 int,
    opResult1 int) {
    
    act := parseAction(input)
    
    assertEqual(t, expectedStartX, act.startX, "")
    assertEqual(t, expectedStartY, act.startY, "")
    assertEqual(t, expectedEndX, act.endX, "")
    assertEqual(t, expectedEndY, act.endY, "")  
    
    var testOpInput int
    
    testOpInput = 0
    act.operation(&testOpInput)
    assertEqual(t, opResult0, testOpInput, "")
    
    testOpInput = 1
    act.operation(&testOpInput)
    assertEqual(t, opResult1, testOpInput, "")  
}

func applyToTest(
    t *testing.T,
    act action,
    input [][]int,
    expected [][]int) {
    
    act.applyTo(input);  
    
    assertEqual(t, len(expected), len(input), "")
    
    for y := 0; y < len(expected); y++ {
        assertEqual(t, len(expected[y]), len(input[y]), "")
        
        for x := 0; x < len(expected[y]); x++ {
            assertEqual(t, 
                expected[y][x], input[y][x], 
                "[" + strconv.Itoa(x) + "," + strconv.Itoa(y) +"]: ")
        }
    }
}

func assertEqual(t *testing.T, expected int, actual int, msg string) {
    if expected != actual {
        t.Error(msg, "Expected: ", expected, "; got: ", actual);
    }
}