-- contains a data type for representing a statement, a statement parser, 
-- a function to interpret a list of statements, and a function for converting the representation to a string.

{- 
 statement ::= variable ':=' expr ';'
           | 'skip' ';'
           | 'begin' statements 'end'
           | 'if' expr 'then' statement 'else' statement
           | 'while' expr 'do' statement
           | 'read' variable ';'
           | 'write' expr ';'
-}

module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail, read)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Begin [Statement] |
    Skip |
    If Expr.T Statement Statement |    
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e -- Variable Expr

begin = accept "begin" -# iter parse #- require "end" >-> buildBeg
buildBeg es = Begin es

skip = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

ifElse = accept "if" -# Expr.parse # require "then" -# parse  # require "else" -# parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

while = accept "while" -# Expr.parse # require "do" -# parse  >-> buildWhile
buildWhile (e, s) = While e s

-- accept "read" "read k;" -> Just("read", "k;")
-- Sends remainder to word which breaks out k and keeps this as result. Then requires ; and just keeps k. the k is then used in Read k. 
read = accept "read" -# word #- require ";" >-> buildRead
buildRead r = Read r

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (Assignment var expr: stmts) dict input =
    exec stmts (Dictionary.insert (var, Expr.value expr dict) dict) input
exec ((Begin ss): stmts) dict input = 
    exec (ss ++ stmts) dict input
exec ((Skip): stmts) dict input = 
    exec stmts dict input
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec ((While cond stmt): stmts) dict input =
    if (Expr.value cond dict) > 0
    then exec (stmt:(While cond stmt): stmts) dict input
    else exec stmts dict input
exec ((Read s): stmts) dict (i:input) =
    exec stmts (Dictionary.insert (s, i) dict) input
exec ((Write e): stmts) dict input = 
    Expr.value e dict : exec stmts dict input

shw :: Statement -> String
shw (Assignment var expr) = var ++ ":=" ++ Expr.toString expr ++ ";\n"
shw (Begin ss) = "begin\n" ++ concat (map shw ss) ++ "end\n"
shw (Skip) = "skip;\n"
shw (If cond thenStmts elseStmts) = "if " ++ Expr.toString cond ++ " then\n" ++ shw thenStmts ++ "else \n" ++ shw elseStmts
shw (While cond stmt) = "while " ++ Expr.toString cond ++ " do \n" ++ shw stmt
shw (Read s) = "read " ++ s ++ ";\n"
shw (Write e) = "write " ++ Expr.toString e ++ ";\n"

instance Parse Statement where
  parse = assignment ! begin ! skip ! ifElse ! while ! read ! write
  toString = shw
