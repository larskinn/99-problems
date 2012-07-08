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