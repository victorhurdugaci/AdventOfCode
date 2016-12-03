import Numeric (showHex)
import System.IO

move :: [Int] -> Int -> Int -> Char -> Int
move pattern size start direction
    | direction == 'U' && (pattern !! (start - size)) /= 0 = start - size
    | direction == 'D' && (pattern !! (start + size)) /= 0 = start + size
    | direction == 'L' && (pattern !! (start - 1)) /= 0    = start - 1
    | direction == 'R' && (pattern !! (start + 1)) /= 0    = start + 1 
    | otherwise                                          = start

findNextDigit :: [Int] -> Int -> Int -> [Char] -> Int
findNextDigit _ _ start []                  = start
findNextDigit pattern size start (dir:rest) = 
    findNextDigit pattern size (move pattern size start dir) rest

findCode :: [Int] -> Int -> Int -> [String] -> [Int]
findCode _ _ _ []                        = []
findCode pattern size start (moves:rest) = 
    (digit:(findCode pattern size digit rest)) 
        where digit = findNextDigit pattern size start moves

main = do
    content <- readFile "input.txt"
    let moves = lines content

    let pattern3x3 = [0,0,0,0,0] ++ 
                     [0,1,2,3,0] ++
                     [0,4,5,6,0] ++
                     [0,7,8,9,0] ++
                     [0,0,0,0,0]

    let code1 = [ pattern3x3 !! x | x <- findCode pattern3x3 5 13 moves ]
    print code1

    let patternDiamond = [0, 0, 0, 0, 0, 0, 0] ++
                         [0, 0, 0, 1, 0, 0, 0] ++
                         [0, 0, 2, 3, 4, 0, 0] ++
                         [0, 5, 6, 7, 8, 9, 0] ++
                         [0, 0,10,11,12, 0, 0] ++
                         [0, 0, 0,13, 0, 0, 0] ++
                         [0, 0, 0, 0, 0, 0, 0]

    let code2 = [ showHex (patternDiamond !! x) "" | x <- findCode patternDiamond 7 23 moves ]
    print code2