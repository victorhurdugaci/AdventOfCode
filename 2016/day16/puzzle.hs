import Data.Bits

-- https://en.wikipedia.org/wiki/Dragon_curve
dragonCurveNth :: Int -> Bool
dragonCurveNth n = (((n .&. (-1 * n)) `shift` 1) .&. n) /= 0

dragonCurve = map (dragonCurveNth) [1..]

generateNBits :: Int -> [Bool] -> [Bool]
generateNBits n start = take n $ foldr(\bs b -> bs ++ b) [] $ map (\(idx,c) -> (if odd idx then start else start') ++ [c] ) $ zip [1..] dragonCurve 
    where 
    start' = map (not) $ reverse start
    neededBlocks = (n + blockSize) `div` blockSize
    blockSize = 1 + length start

checksum :: Int -> [Bool] -> [Bool]
checksum _ [] = []
checksum n x = blockChecksum ++ (checksum n $ drop checksumBlockSize x)
    where    
    blockChecksum = foldr(\_ c' -> checksum' c') (take checksumBlockSize x) [1..checksumIterations]
    checksum' []       = []
    checksum' (a:b:cs) = (a==b):checksum' cs
    checksumIterations = floor $ logBase 2 $ fromIntegral $n `mod` checksumBlockSize -- total number of iterations is decided by the shortest block
    checksumBlockSize = 2 ^ floor ((log $ fromIntegral n) / (log 2)) -- get the smallest power of two less than n 
    
strToBool str = map (\c -> if c=='0' then False else True) str
boolToStr bool = map (\b -> if b then '1' else '0') bool

main = do
    let start = "01111001100111011"
    
    let discSize1 = 272
    print $ boolToStr $ checksum discSize1 $ generateNBits discSize1 $ strToBool start

    let discSize2 = 35651584
    print $ boolToStr $ checksum discSize2 $ generateNBits discSize2 $ strToBool start