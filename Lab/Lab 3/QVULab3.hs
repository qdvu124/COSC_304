-- Quan Vu
-- COSC 304 Lab C

module QVULab3 where

-- Question 2
monext f bin e [] = e
monext f bin e (x:tail) = bin (f x, monext f bin e tail)

add(x,y) = x + y
eadd = 0
times(x,y) = x * y
etimes = 1

f n = 3 * n + 2
g n = n + 1

faspec1 'a' 0 = 1
faspec1 'b' 0 = 0
faspec1 'a' 1 = 2 
faspec1 'b' 1 = 1 
faspec1 'a' 2 = 3 
faspec1 'b' 2 = 2
faspec1 'a' 3 = 0
faspec1 'b' 3 = 3 

-- Question 3
identity x = x
comp(f, g) a = g (f a)

-- Question 4
fa faspec = monext faspec comp identity 

fa1 = fa faspec1 

test1 = fa1 "abbababbaba" 0
test2 = fa1 "abbababbababba" 1 

--  Question 5
startm faspec str = fa faspec str 0

test3 = startm faspec1 "abbababbababba"

-- Question 6
dfaspec1 = (faspec1, [0,2])
dfa (faspec, finalstates) str = inlist (startm faspec str) finalstates

inlist val [] = False
inlist val (x:list) = (val == x) || inlist val list

dfa1 = dfa dfaspec1
test4 = dfa1 "abbababbaba"
test5 = dfa1 "abbababbababba"
