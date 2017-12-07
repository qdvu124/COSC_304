-- Quan Vu COS304, Lab C

module QVULab8 where

spec1 (0, '#') = (0, '#', 'l')
spec1 (0, 'a') = (1, 'a', 'l')
spec1 (0, 'b') = (1, 'a', 'l')
spec1 (0, 'c') = (1, 'a', 'l')
spec1 (0, 'd') = (1, 'a', 'l')
spec1 (1, '#') = (2, '#', 'r')
spec1 (1, 'a') = (1, 'a', 'l')
spec1 (1, 'b') = (1, 'a', 'l')
spec1 (1, 'c') = (1, 'a', 'l')
spec1 (1, 'd') = (1, 'a', 'l')
spec1 (2, '#') = (100, '#', 'd')
spec1 (2, 'a') = (2, 'a', 'r')
spec1 (2, 'b') = (2, 'b', 'r')
spec1 (2, 'c') = (2, 'c', 'r')
spec1 (2, 'd') = (2, 'd', 'r')

val (head:str, 0) = head
val (head:str, pos) = val (str, pos - 1)

startstring str = "#"++str++"#"

startpos "" = 1
startpos (head:str) = 1 + (startpos str)

str1 = "abcd"

chstr(head:str, ch, 0) = ch:str
chstr(head:str, ch, pos) = head:chstr(str, ch, pos - 1)

move (str, state, ch, pos) = let (newstate, newchar, newdir) = spec1(state, ch) in if newdir == 'l' then (chstr(str, newchar, pos), newstate, val(str, pos - 1), pos - 1) else if newdir == 'r' then (chstr(str, newchar, pos), newstate, val (str, pos + 1), pos + 1) else (chstr(str, newchar, pos), newstate, newchar, pos)

test1 = move(str1,0,'d',3)
test2 = move(str1,1,'c',2)
-- Type structure of move :: (Eq t, Eq a, Num t, Num t1, Num a) => ([Char], t, Char, a) -> ([Char], t1, Char, a)
-- We can call it multiple times since the input is a four tuple, and the output is also a four tuple
test3 = move(move(str1,0,'d',3))

run (str, 100, ch, pos) = (str, 100, ch, pos)
run (str, state, ch, pos) = run (move(str, state, ch, pos))

startrun str = run(startstring str, 0, val(startstring str, startpos str),startpos str)
test4 = startrun str1
test5 = startrun "bbccaa"
test6 = startrun "bb#cd"

