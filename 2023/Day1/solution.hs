import Data.Char ( isDigit )
import Data.Text.Internal.Search ( indices )
import Data.Text (unpack, pack, Text)
import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, elemIndex )

tokens :: [String]
tokens = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

findTokens :: String -> [String]
findTokens s@(x:xs) = foldr (\x acc -> if x `isPrefixOf` s then x : acc else acc) [] tokens ++ findTokens xs
findTokens [] = []

customRead :: String -> Int
customRead val@(_:_:_) = fromMaybe undefined (val `elemIndex` tokens) + 1
customRead val = read val

solutionOne :: [String] -> Int
solutionOne = sum . fmap (read . (\digits -> [head digits, last digits]) . filter isDigit)

solutionTwo :: [String] -> Int
solutionTwo = sum . fmap ((\tokens -> customRead (head tokens) * 10 + customRead (last tokens)) . findTokens)

main :: IO()
main = do
    input <- lines <$> readFile "input.txt"
    putStrLn $ "Solution One result: " ++ show (solutionOne input)
    putStrLn $ "Solution Two result: " ++ show (solutionTwo input)