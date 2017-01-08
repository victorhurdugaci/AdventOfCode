-- This is the Josephus solution https://en.wikipedia.org/wiki/Josephus_problem
steal1 :: Int -> Int
steal1 n = (2 * l) + 1
           where l = n - (2 ^ floor ((log $ fromIntegral n) / (log 2)))

steal2 n
    | n == pow3       = n
    | n <= (2 * pow3) = n - pow3
    | otherwise       = pow3 + 2 * ((n - (2 * pow3)))
    where 
    pow3 = 3 ^ floor ((log $ fromIntegral n) / (log 3)) 

main = do
    let elvesCount = 3014387
    print $ steal1 elvesCount
    print $ steal2 elvesCount