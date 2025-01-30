module Testing where

import Data.List ((\\))
import Data.Set (fromList, toList)

-- combinations :: Eq a => [a] -> Int -> [[a]]
-- combinations _ 0 = [[]]
-- combinations xs k =
--     let
--         curr = combinations xs (k-1)
--     in
--         [comb ++ [item] | comb <- curr, item <- xs \\ comb]

-- isLessThanStarting :: Ord a => [a] -> a -> Bool
-- isLessThanStarting [] _ = True
-- isLessThanStarting (x:xs) y = y < x

-- combinations :: Eq a => [a] -> Int -> [[a]]
-- combinations _ 0 = [[]]
-- combinations xs k = [x : y | y <- combinations xs (k-1) | isLessThanStarting y x]

-- isLessThanStarting :: Ord a => [a] -> a -> Bool
-- isLessThanStarting [] _ = True
-- isLessThanStarting (x:_) y = y < x

-- combinations :: Ord a => [a] -> Int -> [[a]]
-- combinations _ 0 = [[]]
-- combinations xs k = [x : y | x <- xs, y <- combinations (xs) (k-1), isLessThanStarting y x]


isLessThanStarting :: Ord a => [a] -> a -> Bool
isLessThanStarting [] _ = True
isLessThanStarting (x:_) y = y <= x

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates xs = toList $ fromList xs

-- Function to get all possible combinations of 5 cards out of n >= 5 cards
combinations :: Ord a => [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations xs k = removeDuplicates [x : y | x <- xs, y <- combinations (xs \\ [x]) (k-1), isLessThanStarting y x]