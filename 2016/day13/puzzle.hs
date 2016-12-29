import Data.Bits
import qualified Data.HashSet as HS
import Data.Graph.AStar

magicNumber = 1364

isWall :: (Int,Int) -> Bool
isWall (-1,_) = True
isWall (_,-1) = True
isWall (x,y)  = odd $ (popCount n)
                where n = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + magicNumber

distEstimate :: (Int,Int) -> (Int,Int) -> Int
distEstimate (x1,y1) (x2,y2) = (abs $ x1-x2) + (abs $ y1-y2)

getNeighbors :: (Int,Int) -> HS.HashSet (Int,Int)
getNeighbors (x,y) = HS.fromList $ [ n | n <- [(x-1,y),(x+1,y),(x,y-1),(x,y+1)], not $ isWall n ]

fill :: (Int,Int) -> Int -> HS.HashSet (Int, Int) -> HS.HashSet (Int, Int)
fill loc remaining visited
    | (remaining == 0) = visited
    | otherwise        = foldl (\xs x -> HS.union (fill x (remaining-1) (HS.insert x visited)) xs) visited neighbors
    where neighbors = [ n | n <- HS.toList $ getNeighbors loc, not $ HS.member n visited ]

main = do
    let goal = (31,39)
    let (Just solution) = aStar getNeighbors 
                                (\x y -> 1)                 -- distance to neighbor
                                (\x -> distEstimate x goal) -- estimate function
                                (\x -> x == goal)           -- goal evaluator
                                (1,1)                       -- start point
    print $ length solution

    print $ HS.size $ fill (1,1) 50 (HS.fromList [(1,1)])