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

mydropWhile _ [] = []
mydropWhile f (x:xs)
            | (f x) = mydropWhile f xs
            | otherwise = (x:xs)

------------------------------------------------------------------------------------------------------
-- Question 5

-- Write a function aboveFive :: [Integer] -> [Integer] that returns the integers in the list which
-- are greater than 5 in the order in which they appear in the original list. You may not use recursion.
-- You must use higher-order functions.
-- [EXAMPLE] > aboveFive [4,2,8,9,2,4,-10,15,5] -> [8,9,15]

aboveFive :: [Integer] -> [Integer]

aboveFive [] = []
aboveFive (x:xs) = filter (>5) (x:xs)

------------------------------------------------------------------------------------------------------
-- Question 6

-- Consider the following lambda calculus expression: (\abc.cba)c(\x.x)
-- Which of the following expressions could be part of a valid reduction sequence of the above expression? You may perform
-- α conversion only when necessary.

-- a) \a.(\x.x)bc
-- b) \c.c(\x.x)c
-- c) \x.x
-- d) \z.z(\x.x)c

-- Step 0: (\abc.cba)c(\x.x)
-- Step 1: (\abz.zba)c(\x.x)  (Change c variable to z because we will be putting the outside c into the parenthesis and dont want them to have same name)
-- Step 2: (\bz.zbc)(\x.x)    (Swap (\x.x) in for all b's)
-- Step 3: (\z.z(\x.x)c)      (This is the step we are looking for thus D is correct answer)

------------------------------------------------------------------------------------------------------
-- Question 7

-- Convert the functions max and max3 into equivalent CPS functions maxk and max3k. You may use neither
-- max nor max3 in your definition of either of the CPS functions, and both maxk and max3k must be in
-- full Continuation-Passing Style. You may assume primitive functions such as +, -, /, and * do not
-- need to be in CPS. Other functions you wish to use must be defined by you and written in Continuation
-- Passing Style (e.g., mod to modk).

-- max a b = if a > b then a else b
-- max3 a b c = 9 + max a (max b c)

-- (Not 100% sure on these)
-- maxk a b k = if a > b then k a else k b
-- max3k a b c k = 9 + maxk b c (\v -> maxk a v k)

------------------------------------------------------------------------------------------------------
-- Question 8








------------------------------------------------------------------------------------------------------
