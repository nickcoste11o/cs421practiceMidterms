module Lib
    (
    ) where

------------------------------------------------------------------------------------------------------
-- Question 1
-- Write the function dotProduct :: [Int] -> [Int] -> Int that takes two lists of integers and returns
-- their dot product. Mathematically, dotProduct [x1, ..., xn] [y1, ..., yn] = x1y1 + ... + xnyn.

dotProduct :: [Int] -> [Int] -> Int

dotProduct [] [] = 0
dotProduct (x:xs) (y:ys)
                    | (null xs) = (x * y)
                    | otherwise = (x * y) + dotProduct xs ys

------------------------------------------------------------------------------------------------------
-- Question 2 (Don't know if i used tail recursion!!!)

-- Write a function decList :: [Int] -> [Int] that takes a list of integers and decrements every
-- element in the list by 1. You must use tail recursion to do this.

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

cons2list :: List a -> [a]

cons2list Nil = []
cons2list (Cons x xs) = x : cons2list xs

-- Write a function countCons :: List a -> Int that counts the number of Cons in a List. You may
-- not use cons2list in your solution.

countCons :: List a -> Int

countCons Nil = 0
countCons (Cons x xs) = 1 + countCons xs

------------------------------------------------------------------------------------------------------
