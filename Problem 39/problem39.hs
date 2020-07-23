import Data.List
import Data.Maybe

integralRightTriangles :: Int -> Int
integralRightTriangles p = sum [1 | a <- [1..p `div` 3], p*(p-2*a) `mod` (2*(p-a)) == 0]

main :: IO ()
main = do
    print $ (((fst unzipped) !!) . (fromMaybe 0) $ (elemIndex ((maximum . snd) unzipped) (snd unzipped)), maximum $ snd unzipped)
    where unzipped = unzip [(x, integralRightTriangles x) | x <- [1..1000]]