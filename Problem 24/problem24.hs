import Data.List

main :: IO ()
main = print $ (sort . permutations $ [0..9]) !! 999999