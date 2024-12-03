import System.IO;
import Data.List

main :: IO ()
main = do
    (a, b) <- readAndSplitColumns "input.txt"

    let sortedA = sort a
    let sortedB = sort b
    let difference = elementDifference sortedA sortedB
    let total = sum difference
    print total --part 1

    let occurrances = countOccurrances a b
    let similarityScore = zipWith(*) a occurrances
    let occurrancesSum = sum similarityScore
    print occurrancesSum

readAndSplitColumns :: FilePath -> IO ([Int], [Int])
readAndSplitColumns filePath = do
    content <- readFile filePath
    let linesList = lines content
    let column1 = map ((read . head) . words) linesList
    let column2 = map ((read . last) . words) linesList
    return (column1, column2)

elementDifference :: Num a => [a] -> [a] -> [a]
elementDifference = zipWith(\x y -> abs (x - y))

countOccurrances :: [Int] -> [Int] -> [Int]
countOccurrances xs ys = map (`countIn` ys) xs
    where countIn x ys' = length $ filter (== x) ys'

