isqrt :: Int -> Int
isqrt n = floor . sqrt $ (fromIntegral n :: Float)

factors :: Int -> Int
factors n = sum [if isqrt n == x && x^2 == n then 1 else 2 | x <- [1..(isqrt n)], n `mod` x == 0]

primes :: [Int]
primes = 1 : 2 : filter (\x -> factors x == 2) [3,5..]

main :: IO ()
main = print $ primes !! 10001