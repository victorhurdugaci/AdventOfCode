import Data.Char
import qualified Data.Set as Set
import System.IO

data Point = Point Int Int deriving (Eq, Ord, Show)
data Pos = Pos Int Int Int deriving (Show)

posToPoint :: Pos -> Point
posToPoint (Pos x y _) = (Point x y)

splitInput :: String -> [String]
splitInput "" = [""]
splitInput (',':rest) = "" : splitInput rest
splitInput (' ':rest) = "" : splitInput rest
splitInput (c:rest)   = (c : head s) : tail s 
    where s = splitInput rest 

parseInput :: [String] -> [Int]
parseInput []                = []
parseInput ((dir:dist):rest) = (turn:(take distCnt (repeat 0))) ++ (parseInput rest)
    where  turn = if dir == 'L' then -1 else 1
           distCnt = read dist
    
adjustDirection :: Pos -> Int -> Pos
adjustDirection (Pos x y dir) turn = (Pos x y ((dir + turn) `mod` 4))

moveFw :: Pos -> Pos
moveFw (Pos x y dir)
    | dir == 0   = (Pos x (y+1) dir) --north
    | dir == 1   = (Pos (x+1) y dir) --east
    | dir == 2   = (Pos x (y-1) dir) --south
    | otherwise  = (Pos (x-1) y dir) --west

move :: Pos -> Int -> Pos
move p moveType
    | moveType == 0 = moveFw p
    | otherwise = adjustDirection p moveType

moveToEnd :: Pos -> [Int] -> Pos
moveToEnd start []              = start
moveToEnd start (moveType:rest) = moveToEnd (move start moveType) rest 

moveToFirstVisited :: Pos -> [Int] -> Set.Set Point -> Pos
moveToFirstVisited start (moveType:rest) visited
    | Set.member (posToPoint start) visited  = start
    | moveType /= 0                          = moveToFirstVisited (move start moveType) rest visited
    | otherwise                              = moveToFirstVisited (move start moveType) rest (Set.insert (posToPoint start) visited)
  
distToPos :: Pos -> Int
distToPos (Pos x y _) = abs(x) + abs(y)

main = do
    handle <- openFile "input.txt" ReadMode
    input <- hGetContents handle
    
    let moves = parseInput (filter (not . null) (splitInput input))
    
    let distanceToHq1 = distToPos(moveToEnd (Pos 0 0 0) moves)
    let distanceToHq2 = distToPos(moveToFirstVisited (Pos 0 0 0) moves Set.empty)
    print ("Distance from HQ1: " ++ show(distanceToHq1))
    print ("Distance from HQ2: " ++ show(distanceToHq2))

    hClose handle
