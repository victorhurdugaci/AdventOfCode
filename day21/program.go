package main

import (
    "fmt"
)

type item struct {
    cost int
    damage int
    armor int
}

var weapons map[string]item = map[string]item {
    "Dagger": item { cost: 8, damage: 4, armor: 0  },
    "Shortsword": item { cost: 10, damage: 5, armor: 0  },
    "Warhammer": item { cost: 25, damage: 6, armor: 0  },
    "Longsword": item { cost: 40, damage: 7, armor: 0  },
    "Greataxe": item { cost: 74, damage: 8, armor: 0  },
}

var armors map[string]item = map[string]item {
    "None": item { cost: 0, damage: 0, armor: 0  }, 
    "Leather": item { cost: 13, damage: 0, armor: 1  },
    "Chainmail": item { cost: 31, damage: 0, armor: 2  },
    "Splintmail": item { cost: 53, damage: 0, armor: 3  },
    "Bandedmail": item { cost: 75, damage: 0, armor: 4  },
    "Platemail": item { cost: 102, damage: 0, armor: 5  },
}

var rings map[string]item = map[string]item {
    "None": item { cost: 0, damage: 0, armor: 0  },
    "Damage +1": item { cost: 25, damage: 1, armor: 0  },
    "Damage +2": item { cost: 50, damage: 2, armor: 0  },
    "Damage +3": item { cost: 100, damage: 3, armor: 0  },
    "Defense +1": item { cost: 20, damage: 0, armor: 1  },
    "Defense +2": item { cost: 40, damage: 0, armor: 2  },
    "Defense +3": item { cost: 80, damage: 0, armor: 3  },
}

const bossHitPoints int = 109
const bossDamage int = 8
const bossArmor int = 2

const playerHitPoints int = 100

func main() {  
   minCost := 1000
   maxCost := 0
   for _, w := range(weapons) {
       for _, a := range(armors) {
           for r1Name, r1 := range(rings) {
               for r2Name, r2 := range(rings) {
                   if (r1Name != "None" && r1Name == r2Name) {
                       // Cannot have same ring twice
                       continue
                   }
                   
                   survived, cost := battle([]item { w, a, r1, r2 })
                   if (survived) {
                       minCost = min(minCost, cost)
                   } else {
                       maxCost = max(maxCost, cost)
                   }
               }
           }
       }
   }
   fmt.Printf("Win min cost: %d\n", minCost)
   fmt.Printf("Lost max cost: %d\n", maxCost)
}

func battle(slots []item) (bool, int) {
    itemsCost := 0
    playerDamage := 0
    playerArmor := 0
    
    for i := 0; i < len(slots); i++ {
        itemsCost += slots[i].cost
        playerDamage += slots[i].damage
        playerArmor += slots[i].armor
    }
 
    playerHP := playerHitPoints
    bossHP := bossHitPoints
    
    playerTurn := true
    for playerHP > 0 && bossHP > 0 {
        if (playerTurn) {
            bossHP -= max(1, playerDamage - bossArmor)
        } else {
            playerHP -= max(1, bossDamage - playerArmor)
        }
        
        playerTurn = !playerTurn
    }
    
    return playerHP > 0, itemsCost
}

func min(a int, b int) int {
    if (a < b) {
        return a
    }
    return b
}
func max(a int, b int) int {
    if (a > b) {
        return a
    }
    return b
}