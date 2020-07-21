isqrt :: Int -> Int
isqrt n = floor . sqrt $ (fromIntegral n :: Float)

sumProperDivisors :: Int -> Int
sumProperDivisors n = sum [if x == 1 then 1 else if x^2 == n then x else x+(n `div` x) | x <- [1..isqrt(n)], n `mod` x == 0]

amicableNumbers :: [Int]
amicableNumbers = [x | x <- [1..9999], let spdx = sumProperDivisors x, sumProperDivisors spdx == x, x /= spdx]

main :: IO ()
main = print (sum amicableNumbers)