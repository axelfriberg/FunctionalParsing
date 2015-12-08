{- Testfor Statement -}
module TestStatement where

import Statement
p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11 :: Statement.T 
p1 = fromString "skip;"
p2 = fromString "read count;"
p3 = fromString "write count+1;"
p4 = fromString "count := 0;"
p5 = fromString "begin skip; end"
p6 = fromString "begin x:=0; x:=x+1; end"
p7 = fromString "if x then skip; else x:=0-x;"
p8 = fromString "while n do n:=n-1;"
s9 = "while n do begin fac:=fac*n; n:=n-1; end"
p9 = fromString s9
p10 = fromString  "begin read x ; x := x + 1 ; write x; end"
p11 = fromString  ("begin read n; fac:=1; " ++ s9 ++ " write fac; end")



