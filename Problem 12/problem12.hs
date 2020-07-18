isqrt :: Int -> Int
isqrt n = floor . sqrt $ (fromIntegral n :: Float)

triangleNumbers :: [Int]
triangleNumbers = [(n*(n+1)) `div` 2 | n <- [1..]]

divisorCount :: Int -> Int
divisorCount n = sum [if isqrt n == x && x^2 == n then 1 else 2 | x <- [1..(isqrt n)], n `mod` x == 0]

main :: IO ()
main = print . head $ dropWhile (\x -> not (divisorCount x > 500)) triangleNumbers