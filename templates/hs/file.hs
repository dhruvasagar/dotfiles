solve :: [Int] -> Int
solve = undefined

main :: IO ()
main = interact $ show . solve . map read . words
