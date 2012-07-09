import Data.List (group)
-- Problem 11:
-- Modified run-length encoding.
data Encoded a = Single a | Multiple Int a deriving (Show)
encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified xs = map encoder $ group xs
                    where encoder [y] = Single y
                          encoder ys@(y:_) = Multiple (length ys) y

-- Problem 12:
-- Decode a run-length encoded list (encoded as in problem 11).
decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap decoder
                 where decoder (Single x) = [x]
                       decoder (Multiple n x) = replicate n x

-- Problem 13:
-- Run-length encoding of a list (direct solution, encoded as in problem 11).
encodeDirect :: Eq a => [a] -> [Encoded a]
-- There is probably a nicer way to do this. Will need to get back to it.
encodeDirect (x:xs) = enc 1 x xs
                      where enc n current [] = [encoded n current]
                            enc n current (x:xs)
                                | current == x = enc (n+1) current xs
                                | otherwise = encoded n current : enc 1 x xs
                            encoded 1 x = Single x
                            encoded n x = Multiple n x

-- Problem 14:
-- Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Problem 15:
-- Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = rpl xs n
           where rpl [] _ = []
                 rpl (_:ys) 0 = rpl ys n
                 rpl ys@(y:_) m = y : rpl ys (m-1)
-- or just
repli' xs n = concatMap (replicate n) xs

-- Problem 16:
-- Drop every N'th element from a list. 
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = drp xs n 1
                 where drp [] _ _ = [] 
                       drp (x:xs) n idx
                           | idx == n = drp xs n 1
                           | otherwise = x : drp xs n (idx+1)
-- or using map, filter and zip:
dropEvery' xs n = map snd $ filter ((/= 0) . (`mod` n) . fst) $ zip [1..] xs


-- Problem 17:
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n (x:xs) | n > 0 = x : myTake (n-1) xs
                | otherwise = []

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (x:xs) | n > 0 = myDrop (n-1) xs
                | otherwise = x:xs

split :: [a] -> Int -> ([a], [a])
split xs n = (myTake n xs, myDrop n xs)

-- Problem 18:
-- Extract a slice from i to k, inclusive, from a list. Start counting the elements with 1.
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k-i+1) $ drop (i-1) xs

-- Problem 19:
-- Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs n | n < 0 = rotate xs (length xs + n)
            | otherwise = drop n xs ++ take n xs

-- Problem 20:
-- Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let start = take (n-1) xs
                    end = drop (n-1) xs
                in (head end, start ++ tail end)
