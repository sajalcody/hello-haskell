-- Do not use any other modules.
module Stream where
import Prelude hiding (find)
-- A Stream is an infinite list.
data Stream a = a :| Stream a
instance Functor Stream where
  fmap = undefined
instance Applicative Stream where
  pure = undefined
  (<*>) = undefined
instance Monad Stream where
  (>>=) = undefined
-- Find the first element of the stream which satisfies a predicate
find :: (a -> Bool) -> Stream a -> a
find pred (x:|xs) 
  | pred x = x 
  | otherwise = find pred xs
-- Find the element of the stream which is n ()'s after the first element which satisfies a predicate
findNthAfter :: (a -> Bool) -> Int  -> Stream a -> a
findNthAfter = undefined --pred 0 xs = find pred xs
-- findNthAfter
filterBy :: (a -> Bool) -> Stream a -> Stream a
filterBy = undefined
take :: Int -> Stream a -> [a]
take 0 xs = []
take n (x:|xs) = x : Stream.take (n - 1) xs
-- Back to plain old Lists
--
-- Find the n'th Largest element in a List.
-- Should not be more than a single pass over the List
nthLargest :: Ord a => Int -> [a] -> Maybe a
nthLargest n xs
  | n == 0 = Nothing
  | n > length xs = Nothing
  | otherwise = do
      let sortedXs = bubbleSort xs
      Just $ sortedXs !! (n - 1)

mode :: (Num a, Eq a) => [a] -> Maybe a
mode xs
  | null xs = Nothing
  | length xs == 1 = Just $ head xs
      | otherwise = do
        let eleCounts = map (\x -> (x, countOccurence x xs)) xs
        Just $ fst $ foldl (\(m, mCount) (x, xCount) -> if xCount > mCount then (x,xCount) else (m, mCount)) (0,0) eleCounts

-- -- takeTillIncluding 11 [2,4,6,8,10,11,12,13,14] == [2,4,6,8,10,11]
takeTillIncluding :: Eq a => a -> [a] -> [a]
takeTillIncluding _ [] = []
takeTillIncluding x xs = do
  let t = takeWhile (x /=) xs 
  if length t == length xs then t else t ++ [x]
  --(foldl (\pos a -> if x == a then pos ++ [a] else pos) [] xs

-- groupEveryNth 5 [1..10] == [[5,10],[1,6],[2,7],[3,8],[4,9]]
-- groupEveryNth :: Int -> [a] -> [[a]]
groupEveryNth 0 xs = map (: []) xs
groupEveryNth i xs= 
  snd $ 
    foldl (\(pos, g) ele->
      if pos <= i then (pos + 1,g ++ [[ele]])
      else do
            let bucket = mod pos i
            if bucket /= 0 then do
            let (prefix,suffix) = splitAt (bucket - 1) g
            (pos + 1, prefix ++ [head suffix ++ [ele]] ++ tail suffix)
            else do
            let (prefix,suffix) = splitAt (i - 1) g
            (pos + 1,prefix ++ [head suffix ++ [ele]])) (1, []) xs
-- Bonus. In case you've done all the above.
-- allCombinationsOfSize 2 "abc" = ["ab", "ac", "bc"] -- Order matters
allCombinationsOfSize :: Ord a => Int -> [a] -> [[a]]
allCombinationsOfSize = undefined

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs) = if sorted thisSort then thisSort else bubbleSort thisSort
    where thisSort = (min x y) : bubbleSort ((max x y):xs)

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False

countOccurence :: Eq a => a -> [a] -> Int
countOccurence x xs
  | null xs = 0
  | otherwise = foldl (\count a -> if a == x then count + 1 else count) 0 xs