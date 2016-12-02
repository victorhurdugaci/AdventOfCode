package main

import (
    "testing"
)

func Test_move_up(t *testing.T) {
    moveTest(t, 0, 0, '^', 1, 0)
    moveTest(t, 1, 0, '^', 2, 0)
    moveTest(t, 0, 1, '^', 1, 1)
    moveTest(t, -45, 123, '^', -44, 123)
    moveTest(t, 45, -123, '^', 46, -123)
}

func Test_move_right(t *testing.T) {
    moveTest(t, 0, 0, '>', 0, 1)
    moveTest(t, 1, 0, '>', 1, 1)
    moveTest(t, 0, 1, '>', 0, 2)
    moveTest(t, -45, 123, '>', -45, 124)
    moveTest(t, 45, -123, '>', 45, -122)
}

func Test_move_down(t *testing.T) {
    moveTest(t, 0, 0, 'v', -1, 0)
    moveTest(t, 1, 0, 'v', 0, 0)
    moveTest(t, 0, 1, 'v', -1, 1)
    moveTest(t, -45, 123, 'v', -46, 123)
    moveTest(t, 45, -123, 'v', 44, -123)
}


func Test_move_left(t *testing.T) {
    moveTest(t, 0, 0, '<', 0, -1)
    moveTest(t, 1, 0, '<', 1, -1)
    moveTest(t, 0, 1, '<', 0, 0)
    moveTest(t, -45, 123, '<', -45, 122)
    moveTest(t, 45, -123, '<', 45, -124)
}

func Test_countVisitedHouses(t *testing.T) {
    assertEqual(t, 1, countVisitedHouses(""))
    
    assertEqual(t, 2, countVisitedHouses("^"))
    assertEqual(t, 2, countVisitedHouses("v"))
    assertEqual(t, 2, countVisitedHouses("<"))
    
    assertEqual(t, 3, countVisitedHouses("^^"))
    assertEqual(t, 3, countVisitedHouses(">>"))
    assertEqual(t, 3, countVisitedHouses("vv"))
    assertEqual(t, 3, countVisitedHouses("<<"))
    
    assertEqual(t, 2, countVisitedHouses("<><><><><>"))
    
    // From puzzle text
    assertEqual(t, 2, countVisitedHouses(">"))
    assertEqual(t, 2, countVisitedHouses("^v^v^v^v^v"))
    assertEqual(t, 4, countVisitedHouses("^>v<"))
}

func Test_countVisitedHousesRobo(t *testing.T) {
    assertEqual(t, 1, countVisitedHousesRobo(""))
    
    assertEqual(t, 2, countVisitedHousesRobo("^"))
    assertEqual(t, 2, countVisitedHousesRobo("v"))
    assertEqual(t, 2, countVisitedHousesRobo("<"))
    
    assertEqual(t, 2, countVisitedHousesRobo("^^"))
    assertEqual(t, 2, countVisitedHousesRobo(">>"))
    assertEqual(t, 2, countVisitedHousesRobo("vv"))
    assertEqual(t, 2, countVisitedHousesRobo("<<"))
    
    assertEqual(t, 3, countVisitedHousesRobo("^^^"))
    assertEqual(t, 3, countVisitedHousesRobo(">>>"))
    assertEqual(t, 3, countVisitedHousesRobo("vvv"))
    assertEqual(t, 3, countVisitedHousesRobo("<<<"))
    
    assertEqual(t, 11, countVisitedHousesRobo("<><><><><>"))
    
    // From puzzle text
    assertEqual(t, 3, countVisitedHousesRobo("^v"))
    assertEqual(t, 3, countVisitedHousesRobo("^>v<"))
    assertEqual(t, 11, countVisitedHousesRobo("^v^v^v^v^v"))
}

func moveTest(t *testing.T, startX int, startY int, direction byte, expectedX int, expectedY int) {
   pos := position {x: startX, y: startY}
   newPos := pos.move(direction)
   assertEqual(t, expectedX, newPos.x)
   assertEqual(t, expectedY, newPos.y)
}

func assertEqual(t *testing.T, expected int, actual int) {
    if expected != actual {
        t.Error( "Expected: ", expected, "; got: ", actual);
    }
}