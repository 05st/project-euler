digits :: Integer -> [Integer]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = digits $ ((read . concat $ lines contents) :: Integer)
    print . maximum $ [product (take 13 (drop x input)) | x <- [0..(length input)-14]]