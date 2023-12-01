import Data.Char ( isDigit )
import Data.Text.Internal.Search ( indices )
import Data.Text (unpack, pack, Text)
import Control.Monad (liftM2)
import Data.List ( elemIndex )
import Data.Maybe (fromMaybe)

tokens :: [Text]
tokens = fmap pack ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

customRead :: String -> Int
customRead val@(_:_:_) = fromMaybe undefined (pack val `elemIndex` tokens) + 1
customRead val = read val

findTokens :: Text -> [(String, Int)]
findTokens input = concatMap (\x -> liftM2 (,) [unpack x] (indices x input)) tokens

findValue :: [(String, Int)] -> Int
findValue records =
        let (maxDigit, _) = foldr (\x acc -> if snd x > snd acc then x else acc) ("", -1) records in
        let (minDigit, _) = foldr (\x acc -> if snd x < snd acc then x else acc) ("", 100000) records in
        customRead minDigit * 10 + customRead maxDigit

solutionOne :: [String] -> Int
solutionOne = sum . fmap ((read :: String -> Int) . (\digits -> [head digits, last digits]) . filter isDigit)

solutionTwo :: [String] -> Int
solutionTwo = sum . fmap (findValue . findTokens . pack)

main :: IO()
main = do
    input <- lines <$> readFile "input.txt"
    putStrLn $ "Solution One result: " ++ show (solutionOne input)
    putStrLn $ "Solution Two result: " ++ show (solutionTwo input)