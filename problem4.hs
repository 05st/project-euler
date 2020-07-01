main :: IO ()
main = print $ maximum [product | x <- [100..999], y <- [100..999], let product = x*y, (reverse $ show product) == show product]