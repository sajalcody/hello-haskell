module Pow where

-- NO GUARDS WITH CASE EXPRESSIONS
-- Calculates power of 2

-- Using Guards
pow2 n
 | n == 0 = 1
 | otherwise = 2 * pow2 (n - 1)

-- Using Pattern Matching
pow2' 0 = 1
pow2' n = 2 * pow2' (n - 1)

-- Imperative Style
pow2'' n = 
  if n == 0
  then 1
  else 2 * pow2'' (n - 1)


-- Remove odd integers from a list

-- Imperative Style
removeOdds nums = 
  if null nums
  then nums
  else
    if mod (head nums) 2 == 0
    then head nums : removeOdds (tail nums)
    else removeOdds (tail nums)

-- Using Pattern Matching & Guards
removeOdds' [] = []
removeOdds' (x:xs)
  | mod x 2 == 0 = x : removeOdds' xs
  | otherwise = removeOdds' xs

-- Doubles every element of the list

double [] = []
double (x:xs) = 2 * x : double xs

double' xs = map (2 *) xs
double'' = map (2 *)
