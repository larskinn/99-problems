import Data.List

-- Problem 1:
-- Find the last element of a list
myLast :: [a] -> a
myLast = foldl1 (\_ x -> x)

-- Problem 2:
-- Find the last but one element of a list
myButLast :: [a] -> a
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

-- Problem 3:
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt xs k = (head . drop (k-1)) xs

-- Problem 4:
-- Find the number of elements of a list
myLength :: [a] -> Int
myLength = sum . map (const 1)
--alternative:
myLength' = foldr (\_ n -> n + 1) 0

-- Problem 5:
-- Reverse a list
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Problem 6:
-- Find out whether a list is a palindrome.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7:
-- Flatten a nested list structure. 
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat $ map flatten xs

-- Problem 8:
-- Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress = foldr noDups []
           where noDups x [] = [x]
                 noDups x (y:ys)
                        | x == y    = y:ys
                        | otherwise = x:y:ys

-- of course, this is easily solved using Data.List.group:
compress' xs = map head $ group xs
-- another alternative is
compress'' [] = []
compress'' (x:xs) = x : (compress'' $ dropWhile (x==) xs)

-- Problem 9:
-- Pack consecutive duplicates of list elements into sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs@(x:_) = (takeWhile (x==) xs) : pack (dropWhile (x==) xs)
-- This exactly the same as Data.List.group

-- Problem 10:
-- Run-length encoding of a list.
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\ys -> (length ys, head ys)) $ pack xs
