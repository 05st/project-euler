isqrt :: Int -> Int
isqrt n = floor . sqrt $ (fromIntegral n :: Float)

factors :: Int -> Int
factors n = sum [if isqrt n == x && x^2 == n then 1 else 2 | x <- [1..(isqrt n)], n `mod` x == 0]

primeFactors :: Int -> [Int]
primeFactors n = [x | x <- [2..(isqrt n)], n `mod` x == 0, factors x == 2]

main :: IO ()
main = print . maximum $ primeFactors 600851475143