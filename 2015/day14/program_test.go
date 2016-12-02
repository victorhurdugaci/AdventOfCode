package main

import (
    "fmt"
    "testing"
)

func Test_moveRaindeer(t *testing.T) {
    // From puzzle text
    comet := raindeer {
        name: "Comet",
        speed: 14,
        speedTime: 10,
        restTime: 127,
    }
    
    for time := 0; time < 138; time++ {
        if (time < 10) {
            moveRaindeerTest(t, &comet, time, 14 * (time + 1))
        } else if (time < 137) {
            moveRaindeerTest(t, &comet, time, 140)
        } else {
            moveRaindeerTest(t, &comet, time, 154)
        }
    }
}

func Test_calculateRaindeerDistance(t *testing.T) {
    // From puzzle text
    comet := raindeer {
        name: "Comet",
        speed: 14,
        speedTime: 10,
        restTime: 127,
    }
    dancer := raindeer {
        name: "Dancer",
        speed: 16,
        speedTime: 11,
        restTime: 162,
    }
    
    calculateRaindeerDistanceTest(t, &comet, 10, 140)
    calculateRaindeerDistanceTest(t, &dancer, 10, 160)
    
    comet = raindeer {
        name: "Comet",
        speed: 14,
        speedTime: 10,
        restTime: 127,
    }
    dancer = raindeer {
        name: "Dancer",
        speed: 16,
        speedTime: 11,
        restTime: 162,
    }
    calculateRaindeerDistanceTest(t, &comet, 1000, 1120)
    calculateRaindeerDistanceTest(t, &dancer, 1000, 1056)
}

func Test_parseRaindeer(t *testing.T) {
    parseRaindeerTest(t, 
        "Vixen can fly 19 km/s for 7 seconds, but then must rest for 124 seconds.",
        "Vixen", 19, 7, 124);
        
    parseRaindeerTest(t, 
        "Prancer can fly 25 km/s for 6 seconds, but then must rest for 143 seconds.",
        "Prancer", 25, 6, 143);
}

func calculateRaindeerDistanceTest(t *testing.T, r *raindeer, time int, expectedDistance int) {
    actualDistance := calculateRaindeerDistance(r, time)
    assertEqual(t, expectedDistance, actualDistance)
} 

func parseRaindeerTest(t *testing.T, input string, expectedName string, expectedSpeed int, expectedSpeedTime int, expectedRestTime int) {
    r := parseRaindeer(input)
    assertEqual(t, expectedName, r.name)
    assertEqual(t, expectedSpeed, r.speed)
    assertEqual(t, expectedSpeedTime, r.speedTime)
    assertEqual(t, expectedRestTime, r.restTime)
}

func moveRaindeerTest(t *testing.T, r *raindeer, time int, expectedDistance int) {
    actualDistance := moveRaindeer(r, time)
    assertEqual(t, expectedDistance, r.distance)
    assertEqual(t, expectedDistance, actualDistance)
}


func calculateRaindeerDistance(r *raindeer, time int) int{
   for t := 0; t < time; t++ {
       moveRaindeer(r, t)
   }
   
   return r.distance
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}