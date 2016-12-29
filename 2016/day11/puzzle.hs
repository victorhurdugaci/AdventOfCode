import Data.Bits
import Data.Char (intToDigit)
import Numeric (showIntAtBase)
import qualified Data.HashSet as HS
import Data.Graph.AStar

-- A building configuration is an int
-- A floor configuration is 
--     - 1 bit for elevator
--     - 2 * elem count bits in the building
--     - odd bits are chips
--     - even bits are generators
--     - the least significant 2 * elem count are the top floor

-- CHANGE THIS TO 7 FOR PART 2
elemCount = 5

floorCount = 4
objCount = 2 * elemCount
objElevCount = objCount + 1

allGenPattern = sum [1 `shift` x | x <- [1,3..(objCount-1)] ] :: Int
allChipPattern = sum [1 `shift` x | x <- [0,2..(objCount-1)] ] :: Int
justElevPattern = 1 `shift` objCount
justObjPattern = justElevPattern - 1
floorPattern = ((1 `shift` objElevCount) -1 ) 

floorOffset :: Int -> Int
floorOffset n = (floorCount - (n+1)) * objElevCount

getNthFloor :: Int -> Int -> Int
getNthFloor n building = (building .&. (floorPattern `shift` offset)) `shift` (-offset)
                         where
                         offset = objElevCount * (floorCount - (n + 1))

getElevatorFloor :: Int -> Int
getElevatorFloor building = loop building 0
                            where
                            loop b idx = if (b .&. (justElevPattern `shift` (floorOffset idx))) /= 0 then idx else loop b (idx+1)

isSolution :: Int -> Bool
isSolution building = (building .&. floorPattern) == building

estimateToSolution :: Int -> Int
estimateToSolution building = sum [ popCount (justObjPattern .&. (getNthFloor n building)) * (floorCount - (n+1)) | n <- [0..(floorCount-1)] ]

isValidFloor :: Int -> Int -> Bool
isValidFloor n b = allGeneratorsOnFloor == 0 ||  -- no generators
                   allChipsOnFloor == 0 ||       -- no chips
                   allChipsWithoutGenerator == 0 -- all chips have generators
                   where 
                   allChipsWithoutGenerator = allObjWithoutPair .&. allChipsOnFloor
                   allObjWithoutPair = (allGeneratorsOnFloor `shift` (-1)) `xor` allChipsOnFloor
                   allChipsOnFloor = floor .&. allChipPattern
                   allGeneratorsOnFloor = floor .&. allGenPattern
                   floor = justObjPattern .&. (getNthFloor n b)

isValidBuilding :: Int -> Bool
isValidBuilding building = and [ isValidFloor x building | x <- [0..(floorCount-1)] ]

getNeighbors :: Int -> HS.HashSet Int
getNeighbors building = HS.fromList [ n | n <- (upMoves ++ downMoves), isValidBuilding n ]
                        where 
                        upMoves = if elevFloor < (floorCount-1) then [ moveObj m 1 building | m <- possibleMoves ] else []
                        downMoves = if elevFloor > 0 then [ moveObj m (-1) building | m <- possibleMoves ] else []
                        possibleMoves = getMovePatterns building
                        elevFloor = getElevatorFloor building

moveObj :: Int -> Int -> Int -> Int
moveObj movePattern plusMinus building = (building .&. (complement $ movePattern)) .|. (movePattern `shift` ((-plusMinus) * objElevCount))

getMovePatterns :: Int -> [Int]
getMovePatterns building = oneObj ++ twoObj
                           where
                           twoObj = [ x .|. y | x <- oneObj, y <- oneObj, x < y ] -- x < y is a hacky distinct
                           oneObj = [ (justElevPattern .|. (1 `shift` n)) `shift` (floorOffset elevFloor) | n <- [0..(objCount-1)], (floor .&. (1 `shift` n) /= 0) ]
                           floor = getNthFloor elevFloor building
                           elevFloor = getElevatorFloor building

main = do
    let b =  (0 `shift`    (0 * objElevCount)) +   -- 0 00000 00000
             (0 `shift`    (1 * objElevCount)) +   -- 0 00000 00000
             (272 `shift`  (2 * objElevCount)) +   -- 0 01000 10000
             (1775 `shift` (3 * objElevCount))     -- 1 10111 01111

    -- PART 2 (see the parameter at top that has to be changed)
    -- let b =  (0 `shift`    (0 * objElevCount)) +   -- 0 00000 00000 0000
    --          (0 `shift`    (1 * objElevCount)) +   -- 0 00000 00000 0000
    --          (4352 `shift`  (2 * objElevCount)) +  -- 0 01000 10000 0000
    --          (28415 `shift` (3 * objElevCount))    -- 1 10111 01111 1111

    let (Just solution) = aStar getNeighbors (\x y -> 1) estimateToSolution isSolution b
    print $ length solution