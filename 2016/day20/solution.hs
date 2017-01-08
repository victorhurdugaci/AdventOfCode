import Data.List.Split
import Data.List (sortBy)
import System.IO

findLowestUnblocked :: Int -> [(Int,Int)] -> Int
findLowestUnblocked n [] = n
findLowestUnblocked n ((a,b):xs)
    | (n < a)   = n
    | (b < n)   = findLowestUnblocked n xs
    | otherwise = findLowestUnblocked (b+1) xs

countUnblocked :: Int -> [(Int, Int)] -> Int
countUnblocked n [] = 4294967295 - n
countUnblocked n ((a,b):xs)
    | n > 4294967295 = 0
    | (n < a)   = (a - n) + countUnblocked (b+1) xs
    | (b < n)   = countUnblocked n xs
    | otherwise = countUnblocked (b+1) xs

main = do
    input <- readFile "input.txt"
    let filters = map (\x -> (read $ x !! 0, read $ x !! 1)::(Int,Int)) $ map (\x -> splitOn "-" x) $ lines input
    let sorterdFilters = sortBy (\(a1,b1) (a2,b2) -> if a1 /= a2 then compare a1 a2 else compare b2 b1) filters

    -- Part 1 --
    print $ findLowestUnblocked 0 sorterdFilters

    -- Part 2 --
    print $ countUnblocked 0 sorterdFilters