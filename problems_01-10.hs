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
