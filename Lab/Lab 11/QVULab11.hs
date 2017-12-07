-- Quan Vu COS304, Lab C

module QVULab11 where

helperab [] = []
helperab (str:rest) = ("a"++str++"b"):helperab(rest)

listab = "" : helperab listab
test1 = helperab ["cat", "dog"]
test2 = take 5 listab

helperc [] = []
helperc (str:rest) = (str++"c"):helperc(rest)

listc = "" : helperc listc
test3 = take 5 listc

mergelists [] [] = []
mergelists (a:rest1) (b:rest2) = (a ++ b) : mergelists (rest1) (rest2)
mergelists (a:rest1) []  = a : mergelists (rest1) []
mergelists [] (b:rest2) = b : mergelists [] (rest2)

test4 = mergelists ["ab", "cd", "ef" , "g"] ["cat", "dog", "bird"]

listabc = mergelists listab listc
test5 = take 5 listabc

helperabc [] = []
helperabc (str:rest) = ("a"++str++"bc"):helperabc(rest)
test6  = helperabc ["cat", "dog"]

mixedabc = "" : helperabc mixedabc
test7 = take 5 mixedabc

swap "" = ""
swap ('a':rest) = "a"++swap(rest)
swap ('b':rest) = "b"++swap(rest)
swap ('c':'b':rest) = "b"++swap("c"++rest)
swap ('c':'c':rest) = swap(rest)++"cc"
swap [x] = [x]

swaplist [] = []
swaplist (str:rest) = swap(str):swaplist(rest)

listabc1 = swaplist mixedabc
test8 = take 5 listabc1
