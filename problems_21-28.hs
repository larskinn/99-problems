import System.Random
import Control.Monad (replicateM)

-- Problem 21:
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x all@(y:ys) n | n <= 1 = x : all
                        | n  > 1 = y : insertAt x ys (n-1)

-- Problem 22:
-- Create a list containing all integers within a given range.
range :: Integral n => n -> n -> [n]
range m n = [m..n]

-- Problem 23:
-- Extract a given number of randomly selected elements from a list.
-- (I'm assuming the extracted elements don't need to be distinct)
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect xs n = replicateM n (get1 xs)
                 where get1 xs = do gen <- newStdGen
                                    let (idx, _) = randomR (0, length xs - 1) gen
                                    return (xs !! idx)

