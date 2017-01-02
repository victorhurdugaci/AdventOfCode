import System.IO

safePad input = "." ++ input ++ "."

computeTile :: Char -> Char -> Char -> Char
computeTile l c r = if (l /= r) then '^' else '.'

nextRow :: String -> String 
nextRow row = nextRow' row
              where 
              nextRow' (l:c:r:rest) = (computeTile l c r):(nextRow' $ c:r:rest)
              nextRow' _            = []

room row = row:(room $ nextRow $ safePad row)

safeCount n row  = foldr (\row cnt -> cnt + (length $ filter (\c -> c == '.') row)) 0 $ take n $ room row

main = do
    input <- readFile "input.txt"

    -- Part 1 --
    print $ safeCount 40 input

    -- Part 2 --
    print $ safeCount 400000 input