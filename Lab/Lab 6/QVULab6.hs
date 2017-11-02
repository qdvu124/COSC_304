-- Quan Vu
-- COSC 304 Lab C

module QVULab6 where

monext f bin e [] = e
monext f bin e (x:tail) = bin (f x, monext f bin e tail)

identity x = x
comp(f, g) a = g (f a)

inlist val [] = False
inlist val (x:list) = (val == x) || inlist val list

first (a,b) = a
second (a,b) = b

spec1 'a' (0, stack) = (0, "a"++stack)
spec1 'b' (0, stack) = (0, "b"++stack)
spec1 'c' (0, stack) = (1, stack)
spec1 'a' (1, 'a':stack) = (1, stack)
spec1 'b' (1, 'b':stack) = (1, stack)

final1 = [(1, "")]
pdaspec1 = (spec1, final1)
pdacreate (spec, final) = (monext spec comp identity, final)

pda1 = pdacreate pdaspec1

pdacomp (pdaspec, final) string =  pdaspec string (0, "")
test1 = pdacomp pda1 "abbacab"

pdatest (pdaspec, final) string = inlist (pdacomp (pdaspec, final) string) final

test2 = pdatest pda1 "abbcbba"
-- When we try pdacomp or pdatest on "abbacbbab", we get a non-exhaustive pattern error. 
-- The spec that we defined has no way to deal with the situation when it encounters a b in a final state, while the top of the stack is an 'a'

spec2 'x' (0, "") = (1, "c")
spec2 'a' (1, 'c':stack) = (1, "ac"++stack)
spec2 'a' (1, 'a':stack) = (1, "aa"++stack)
spec2 'a' (1, 'b':stack) = (1, stack)
spec2 'b' (1, 'c':stack) = (1, "bc"++stack)
spec2 'b' (1, 'b':stack) = (1, "bb"++stack)
spec2 'b' (1, 'a':stack) = (1, stack)
spec2 'z' (1, 'c':stack) = (2, "");

final2 = [(2, "")]
pdaspec2 = (spec2, final2)

pda2 = pdacreate pdaspec2

test3 = pdacomp pda2 "xaabbabaz"
test4 = pdatest pda2 "xaabbabaz"

