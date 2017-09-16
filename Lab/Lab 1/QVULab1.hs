-- COSC 304 Fall 2017
-- Lab1
module QVULab1 where

{- 
Input: 8, Output: 8
Input: [2,3], Output: [2,3]
Input: 3.5, Output: 3.5
Input: :type 1, Output: 1 :: (Num t) => t
Input: :type [2,3], Output: [2,3] :: (Num t) => [t]
Input: :type 3.5, Output: 3.5 :: (Fractional t) => t
Input: :type (1, 2.0), Output: (1, 2.0) :: (Num t, Fractional t1) => (t, t1)
-}

double x = 2 * x

{- 
Input: double 2, Output: 4
Input: double 3.5, Output: 7.0
Input: double [2,3], Output: Error ( No instance for (Num [t])
      arising from a use of `double' at <interactive>:1:0-11)
Input: :type double, Output: double :: (Num t) => t -> t
-}

triple x = 3 * x

{- 
Input: triple 1.4, Output: 4.199999999999999
Input: triple [2,3], Output: Error ( No instance for (Num [t])
      arising from a use of `double' at <interactive>:1:0-11)
Input: :type triple, Output: triple :: (Num t) => t -> t
-}

add1 (a, b) = a + b
-- Type Structure: (Num t) => (t, t) -> t

add2 a b = a + b
-- Type Structure: (Num a) => a -> a -> a

fact 0 = 1
fact x = x * (fact(x-1))
-- (Num t) => t -> t

boolval1 = 3 == 4
-- Input: boolval1, Output: False
-- Input: not boolval1, Output: True

first (a,b) = a
second (a,b) = b
{-
Type Structure for first: (t, t1) -> t
Type Structure for second: (t, t1) -> t1
They are polymorphic since they don't have any type associated with the domain
Type Structure for first(add1, fact): (Num t) => (t, t) -> t
The function first(add1, fact) returns add1, however since we do not have any parameters for add1, we get errors
-}

test2 = second(add1, fact)
-- Input: test2 3, Output: 6

listlength [] = 0
listlength (x:list) = 1 + (listlength list)
-- Type Structure: (Num t) => [t1] -> t
