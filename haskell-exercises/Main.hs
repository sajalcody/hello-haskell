module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome = undefined

-- intersperse ',' ["Hello", "World"] = "Hello,World"
-- intersperse ' ' ["a", "b", "c"] = "a b c"
intersperse :: a -> [a] -> [a]
intersperse = undefined

-- coalesce successive white spaces into a single white space
-- coalesceWS "foo    bar  bazz    buzz" = "foo bar bazz buzz"
coalesceWS :: String -> String
coalesceWS = undefined

-- splitAt "foo,bar" ',' = ("foo", "bar")
splitAt :: [a] -> a -> ([a], [a])
splitAt = undefined

data URL
  = URL
    { scheme :: String
    , host :: String
    , port :: String
    , path :: String
    , queryParams :: String
    }

-- Parse a url like http://api.juspay.in:5050/path1/path2?foo=bar
-- Use splitAt as defined above
parseURL :: String -> Maybe URL
parseURL = undefined

isReverse :: [a] -> [a] -> Bool
isReverse = undefined

forWithIndex :: Int -> Int -> (Int -> a -> b) -> a -> [b]
forWithIndex = undefined

-- Most commonly occuring value.
mode :: Ord a => [a] -> Maybe a
mode = undefined

-- Center value or pair of values
median :: Ord a => [ a ] -> Maybe (Either a (a, a))
median = undefined

-- "foo" `eqCaseInsensitive` "FoO" = True
eqCaseInsensitive :: String -> String -> Bool
eqCaseInsensitive = undefined

concat' :: [[a]] -> [a]
concat' = undefined

factorial :: Int -> Integer
factorial = undefined

permutations :: [a] -> [[a]]
permutations = undefined

data FizzBuzzNumbers = I Int | Fizz | Buzz | FizzBuzz

-- See Instructions on https://www.hackerrank.com/challenges/fizzbuzz/problem
generateFizzBuzz :: Int -> Int -> [FizzBuzzNumbers]
generateFizzBuzz = undefined

runLengthEncoding :: Eq a => [a] -> [(Int, a)]
runLengthEncoding = undefined

-- Rotate to the right
rotateList :: Int -> [a] -> [a]
rotateList = undefined
