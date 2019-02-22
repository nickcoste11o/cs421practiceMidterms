module Lib
    (
    ) where

------------------------------------------------------------------------------------------------------
-- Question 1
-- Write the function dotProduct :: [Int] -> [Int] -> Int that takes two lists of integers and returns
-- their dot product. Mathematically, dotProduct [x1, ..., xn] [y1, ..., yn] = x1y1 + ... + xnyn.
-- [EXAMPLE] *Main> dotProduct [0,1,2] [1,2,3] -> 8

dotProduct :: [Int] -> [Int] -> Int

dotProduct [] [] = 0
dotProduct (x:xs) (y:ys)
                    | (null xs) = (x * y)
                    | otherwise = (x * y) + dotProduct xs ys

------------------------------------------------------------------------------------------------------
-- Question 2 (Don't know if i used tail recursion!!!)

-- Write a function decList :: [Int] -> [Int] that takes a list of integers and decrements every
-- element in the list by 1. You must use tail recursion to do this.
-- [EXAMPLE] *Main> decList [1,2,3] -> [0,1,2]

decList :: [Int] -> [Int]

decList [] = []
decList (x:xs)
        | (null xs) = [x-1]
        | otherwise = (x-1) : decList xs

------------------------------------------------------------------------------------------------------
-- Question 3

-- Given the following algebraic data type:

data List a = Cons a (List a)
            | Nil
  deriving Show

-- Write a function cons2list :: List a -> [a] that converts a Cons into a Haskell list.
-- [EXAMPLE] *Mp1> cons2list (Cons 2 (Cons 3 (Cons 4 Nil))) -> [2,3,4]

cons2list :: List a -> [a]

cons2list Nil = []
cons2list (Cons x xs) = x : cons2list xs

-- Write a function countCons :: List a -> Int that counts the number of Cons in a List. You may
-- not use cons2list in your solution.
-- [EXAMPLE] *Mp1> countCons (Cons 2 (Cons 3 (Cons 4 Nil))) -> 3

countCons :: List a -> Int

countCons Nil = 0
countCons (Cons x xs) = 1 + countCons xs

------------------------------------------------------------------------------------------------------
-- Question 4

-- Write the higher order function mydropWhile :: (a -> Bool) -> [a] -> [a] that, given a predicate and
-- a list, drops initial elements from the list while the predicate still holds true. (A predicate is
-- a function that returns a Bool.) You may not use the higher order function dropWhile.
-- [EXAMPLE] Main> mydropWhile (< 3) [1,2,3,4,5,1,2,3] -> [3,4,5,1,2,3]

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile = undefined












------------------------------------------------------------------------------------------------------
