replicate' :: (Num i, Ord i) => i -> n -> [n]
replicate' n x 
   | n <= 0  = []
   | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [n] -> [n]
take' _ [] = []
take' n (x:xs)
   | n <=0 = []
   | otherwise = x:take' (n-1) xs  

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

printer ::(Num a) => [a] -> a
printer (x:a:sa) = a


-- quicksort :: (Ord a) => [a] -> [a]
--quicksort [] = []
--quicksort (x:xs) = quicksort smallList ++ [x] ++ quicksort largeList
 -- where smallList = [y | y <- xs, y < x]
--		largeList = [z | z <- xs, z >= x]
		
collatz :: Integral a => a -> [a]
collatz 1 = [1]
collatz x
	| even x = (x:collatz (div x 2))
	| otherwise = x: collatz (3*x + 1)

countchain = sum (filter (>15) (map length (map collatz [1..100])))