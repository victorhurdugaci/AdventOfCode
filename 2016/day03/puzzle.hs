import Text.Regex.Posix
import System.IO

type Triangle = (Int, Int, Int)

isTriangle :: Triangle -> Bool
isTriangle (a,b,c) = (a+b>c) && (a+c>b) && (b+c>a)

textToTriangle :: String -> Triangle
textToTriangle txt = (read a, read b, read c) :: Triangle
    where  (_, _, _, [a, b, c]) = (txt =~ "([0-9]+)[ ]+([0-9]+)[ ]+([0-9]+)" :: (String,String,String,[String]))

-- creates vertical triangles from horizontal
hToV :: [Triangle] -> [Triangle]
hToV []                                      = []
hToV ((a1,b1,c1):(a2,b2,c2):(a3,b3,c3):rest) = (a1,a2,a3):(b1,b2,b3):(c1,c2,c3):(hToV rest)

countTriangles :: [Triangle] -> Int
countTriangles [] = 0
countTriangles (t:rest) = (if isTriangle t then 1 else 0) + countTriangles rest

main = do
    content <- readFile "input.txt"
    let parsedTriangles = [ textToTriangle t | t <- lines content]

    let triangleCountH = countTriangles parsedTriangles 
    print triangleCountH

    let triangleCountV = countTriangles (hToV parsedTriangles) 
    print triangleCountV
