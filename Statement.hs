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
{-
Define a parsing function for each kind of statement. If the parser has accepted the first reserved word in a statement, 
you should use require rather than accept to parse other reserved words or symbols in order to get better error messages in case of failure. 
An example:
   assignment = word #- accept ":=" # Expr.parse 
                     #- require ";" >-> buildAss
   buildAss (v, e) = Assignment v e
-}

{-
The parser m #- n accepts the same input as m # n, but returns the result from the m parser.
This parser applies two parsers in sequence where the remainder string from the first one is fed into the other, the results of the two parsers are combined into a pair.
Does word function and sends the remainder to accept ":=", 
This parser applies two parsers in sequence where the remainder string from the first one is fed into the other, 
the results of the two parsers are combined into a pair.
The word is then combined with Expr.parse and sent to require
-}
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
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = assignment ! begin ! skip ! ifElse ! while ! read ! write
  toString = error "Statement.toString not implemented"
