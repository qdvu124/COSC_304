-- Quan Vu
-- COSC 304 Lab C

module QVULab2 where

listlength [] = 0
listlength (x:list) = 1 + listlength(list)

listsum [] = 0
listsum (x:list) = x + listsum(list)

data Nat = Z|S Nat deriving (Read, Show, Eq)

add x Z = x
add x (S y) = S (add x y)

minus Z x = Z
minus x Z = x
minus (S x) (S y) = S  (minus x y) 

mult x Z = Z
mult Z x = Z
mult x (S Z) = x
mult x (S y) = add x (mult x y)

nattoInt Z = 0
nattoInt (S x) = 1 + nattoInt(x)

buildNat 0 = Z
buildNat x = S (buildNat (x-1))

lt Z Z = False
lt Z x = True
lt x Z = False
lt (S x) (S y) = lt x y

lte x y = (lt x y) || (x == y)

factNat Z = S Z
factNat (S x) = mult (S x) (factNat x)

natThree = buildNat 3
natFive = buildNat 5
natTwo = buildNat 2

{--


--}