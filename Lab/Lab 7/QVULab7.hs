-- Quan Vu
-- COSC 304 Lab C

module QVULab7 where

monext f bin e [] = e
monext f bin e (x:tail) = bin (f x, monext f bin e tail)

inlist a [] = False
inlist a (x:list) = (a == x) || inlist a list

flatten [] = []
flatten (list:listoflists) = list ++ (flatten listoflists)

relcomp(f, g) a = flatten (map g (f a))
relidentity x = [x]

gspec1 0 'S' = "aSb"
gspec1 1 'S' = ""
gspec1 0 'a' = "a"
gspec1 0 'b' = "b"
gspec1 1 'a' = "a"
gspec1 1 'b' = "b"

buildgram spec = monext spec relcomp relidentity

gramgen1 = (buildgram gspec1, 'S')

startgramgen (gram, startsym) rulelist = gram rulelist startsym

test1 = startgramgen gramgen1 [0, 0, 0, 1]

gspec2 0 'S' = "aSa"
gspec2 1 'S' = "bSb"
gspec2 2 'S' = ""
gspec2 0 'a' = "a"
gspec2 0 'b' = "b"
gspec2 1 'a' = "a"
gspec2 1 'b' = "b"
gspec2 2 'a' = "a"
gspec2 2 'b' = "b"

gramgen2 = (buildgram gspec2, 'S')
test2 =  startgramgen gramgen2 [0, 0, 1, 0, 2, 2, 1, 2]

gspec3 0 'E' = "E+T"
gspec3 1 'E' = "T"
gspec3 2 'T' = "X.T"
gspec3 3 'T' = "X"

gspec3 0 'X' = "X"
gspec3 1 'X' = "X"
gspec3 2 'X' = "X"
gspec3 3 'X' = "X"

gspec3 0 '+' = "+"
gspec3 1 '+' = "+"
gspec3 2 '+' = "+"
gspec3 3 '+' = "+"

gspec3 0 '.' = "."
gspec3 1 '.' = "."
gspec3 2 '.' = "."
gspec3 3 '.' = "."

gspec3 0 'T' = "T"
gspec3 1 'T' = "T"

gspec3 2 'E' = "E"
gspec3 3 'E' = "E"

gramgen3 = (buildgram gspec3, 'E')

test3 = startgramgen gramgen3 [0, 1, 2, 3, 0, 1]
test4 = startgramgen gramgen3 [3, 3, 0, 1, 2, 1]

add [] = []
add (x:s) = (x++[0]):(x++[1]):(x++[2]):(x++[3]):(add s)

star0123 = []:(add star0123)

testrun0123 gramgen n = map (startgramgen gramgen) (take n star0123)
test5 = testrun0123 gramgen3 20

inLgramgen3 str n = inlist str (testrun0123 gramgen3 n)
test6 = inLgramgen3 "X+X.X" 400
test7 = inLgramgen3 "X.X+X" 400
test8 = inLgramgen3 "X.X+X" 800 

