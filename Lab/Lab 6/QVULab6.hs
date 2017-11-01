-- Quan Vu
-- COSC 304 Lab C

module QVULab6 where

monext f bin e [] = e
monext f bin e (x:tail) = bin (f x, monext f bin e tail)

inlist val [] = False
inlist val (x:list) = (val == x) || inlist val list

first (a,b) = a
second (a,b) = b

