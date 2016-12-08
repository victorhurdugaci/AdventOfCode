import Data.List
import Data.Map
import System.IO

strToCharList :: String -> [String]
strToCharList [] = []
strToCharList (c:rest) = [c]:strToCharList rest

freqComparatorAsc :: (Char, Int) -> (Char, Int) -> Ordering
freqComparatorAsc (_, b1) (_, b2) = compare b2 b1

freqComparatorDesc :: (Char, Int) -> (Char, Int) -> Ordering
freqComparatorDesc (_, b1) (_, b2) = compare b1 b2

findFrequent :: String -> ((Char, Int) -> (Char, Int) -> Ordering) -> String
findFrequent txt comparator = 
    [ c | (c, _) <- take 1 (sortBy comparator freqs)] where 
        freqs = (toList $ fromListWith (+) [ (c, 1) | c <- txt, c /= '-' ])

rowsToColumns :: [String] -> [String] -> [String]
rowsToColumns [] cols = cols
rowsToColumns (l:rest) cols = zipWith (++) (strToCharList l) (rowsToColumns rest cols)

main = do
    content <- readFile "input.txt"
    let rows = lines content
    let startCols = take (length (rows !! 0)) (repeat "")
    let cols = rowsToColumns rows startCols
    
    let msg1 = [ findFrequent col freqComparatorAsc | col <- cols ] 
    print msg1

    let msg2 = [ findFrequent col freqComparatorDesc | col <- cols ] 
    print msg2
