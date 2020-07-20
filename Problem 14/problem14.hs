import Data.List
import Data.Ord

maxIndex :: Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]

sequenceLength :: Int -> Int -> Int
sequenceLength 1 l = l
sequenceLength n l
    | even n = sequenceLength (n `div` 2) l+1
    | odd n = sequenceLength (3*n + 1) l+1

main :: IO ()
main = do
    print $ (fst list) !! (maxIndex $ snd list)
    where list = unzip [(x, sequenceLength x 1) | x <- [1..1000000]]