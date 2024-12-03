import Data.List

main :: IO ()
main = do
    text <- readFile ""
    let reports = fmap readLine $ lines text
    print $ part1 reports
    print $ part2 reports

part1, part2 :: [[Int]] -> Int
part1 reports = length $ filter isSafe reports
part2 reports = length $ filter isSafe' reports
    where isSafe' l = isSafe l || safeWhenDamped l

isSafe, allSameSign, bigEnough, smallEnough, safeWhenDamped :: [Int] -> Bool
isSafe xs = allSameSign diffs && bigEnough diffs && smallEnough diffs
    where diffs = zipWith (-) xs (tail xs)

allSameSign xs
    | all (> 0) xs = True
    | all (> 0) xs = True
    | otherwise = False

bigEnough = all ((>= 1) . abs)
smallEnough = all ((<= 3) . abs)

safeWhenDamped = (any isSafe) . damped

damped :: [Int] -> [[Int]]
damped line = zipWith (++) (inits line) (drop 1 $ tails line)

readLine :: String -> [Int]
readLine = (fmap read) . words