import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Digest.Pure.MD5
import Data.Graph.AStar
import qualified Data.HashSet as HS
import Data.Maybe

input = "yjjvjgan"

type Room = (Int, Int)
type Path = (Room, String)

isOpen :: Room -> Char -> Bool
isOpen (x,y) c = (x >= 0 ) && (x < 4) && (y >= 0) && (y < 4) && (c > 'a')

goalEstimate :: Path -> Int
goalEstimate ((x,y),_) =  (3-x) + (3-y)

getNeighbors :: Path -> HS.HashSet Path
getNeighbors ((x,y), path) = HS.fromList $ [ (room, path ++ [dir]) | 
                                             (room, dir, h) <- zipWith3 (\a b c -> (a,b,c)) 
                                                                        [(x-1,y),(x+1,y),(x,y-1),(x,y+1)] 
                                                                        ['U','D','L','R'] 
                                                                        (take 4 $ show $ md5 $ BS.fromString path), 
                                             isOpen room h]

-- Part 1 --
findShortestPath :: Path -> Maybe [Path]
findShortestPath start = aStar getNeighbors                  -- neighbors function
                         (\x y -> 1)                   -- distance to neighbor function
                         (\x -> goalEstimate x)        -- estimate function
                         (\x -> (goalEstimate x) == 0) -- goal evaluator function
                         start                         -- start point

-- Part 2 --
findLongestPath :: Path -> Int ->Int
findLongestPath p pathLen
    | (goalEstimate p) == 0 = pathLen
    | otherwise             = maximum $ 0:(map (\n -> findLongestPath n (pathLen + 1)) $ HS.toList $ getNeighbors p)

main = do
    let start =  ((0,0), input) 

    let shortest = findShortestPath start
    if isNothing shortest then print "No solution"
                          else print $ drop (length input) $ snd $ last $ fromJust shortest

    -- Part 2 --
    print $ findLongestPath start 0
