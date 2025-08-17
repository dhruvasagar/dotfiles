part1 :: [Int] -> Int
part1 = undefined

part2 :: [Int] -> Int
part2 = undefined

main :: IO ()
main = interact $ unlines . map show . sequence [part1, part2] . map read . lines
