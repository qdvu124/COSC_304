-- Quan Vu
-- COSC 304 Lab C

module QVULab6 where

monext f bin e [] = e
monext f bin e (x:tail) = bin (f x, monext f bin e tail)

inlist val [] = False
inlist val (x:list) = (val == x) || inlist val list

first (a,b) = a
second (a,b) = b

spec1 'a' (0, stack) = (0, "a"++stack)
spec1 'b' (0, stack) = (0, "b"++stack)
spec1 'c' (0, stack) = (1, stack)
spec1 'a' (1, 'a':stack) = (1, stack);
spec1 'b' (1, 'b':stack) = (1, stack);
