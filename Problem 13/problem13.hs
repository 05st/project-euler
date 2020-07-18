main :: IO ()
main = do
    contents <- fmap lines $ readFile "input.txt"
    print (take 10 (show . sum $ (map read contents :: [Integer])))