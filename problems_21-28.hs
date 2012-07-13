-- Problem 21:
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x all@(y:ys) n | n <= 1 = x : all
                        | n  > 1 = y : insertAt x ys (n-1)
