import Data.Maybe
import Data.List

diskAtT :: (Int, Int) -> Int -> Int -> Int
diskAtT (totalPos, startPos) discIdx t = (startPos + (t+discIdx)) `mod` totalPos

canFall :: [(Int,Int)] -> Int -> Bool
canFall discs t = and $ map (\(idx, d) -> (diskAtT d idx t) == 0) $ zip [1..] discs 

findFirstT :: [(Int, Int)] -> Int
findFirstT discs = fst $ fromJust $ find (\(t, falls) -> falls) $ [ (t, canFall discs t) | t <- [0..] ]

main = do
    let discs1 = [ (13,11), 
                  (5,0),
                  (17,11),
                  (3,0),
                  (7,2),
                  (19,17) ]
    print $ findFirstT discs1

    let discs2 = discs1 ++ [ (11,0) ]
    print $ findFirstT discs2
    