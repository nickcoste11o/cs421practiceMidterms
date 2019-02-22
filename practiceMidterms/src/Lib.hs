module Lib
    (
    ) where

--------------------------------------------------------------------------------------------
-- Question 2

-- Write a function decList :: [Int] -> [Int] that takes a list of integers and
-- decrements every element in the list by 1. You must use tail recursion to do this.

decList :: [Int] -> [Int]
decList (x:xs)
        | (null xs) = [x-1]
        | otherwise = (x-1) : decList xs
--------------------------------------------------------------------------------------------


--------------------------------------------------------------------------------------------
-- Question 3


data List a = Cons a (List a)
            | Nil
  deriving Show

cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x xs) = x : cons2list xs

countCons :: List a -> Int
countCons Nil = 0
countCons (Cons x xs) = 1 + countCons xs
