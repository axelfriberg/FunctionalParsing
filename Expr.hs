-- contains a data type for representing an arithmetic expression, an expression parser, an expression evaluator, 
-- and a function for converting the representation to a string.
module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary

-- The result of the parser
-- An expression can be either a number, variable, add/sub/mul/div between two expr
data Expr = Num Integer | Var String | Add Expr Expr 
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr :: Parser Expr

term', expr' :: Expr -> Parser Expr

-- String of letters
var = word >-> Var

-- String of digits
num = number >-> Num

-- Parser that will accept * or / and return a Mul or Div
-- mulOp "*2*3 -> Just(Mul, "2*3")
mulOp = lit '*' >-> (\ _ -> Mul) !
        lit '/' >-> (\ _ -> Div)

-- Parser that will accept + or - and return a Add or Sub
addOp = lit '+' >-> (\ _ -> Add) !
        lit '-' >-> (\ _ -> Sub)

-- Used for transformation, takes and expression and a (operator,expr) pair and makes it into a new expression?
-- bldOp (Num 1) (Mul, Num 2) -> Mul Num 1 Num 2
bldOp e (oper,e') = oper e e'

-- Returns a Parser Expr
factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

-- Does mulOp and sends the remainder string to factor which is combined into a pair. This pair is transformed by bldOp. 
-- The transformed result is geven recursively to term' which will finda a new mulOp or terminate.
-- if the input doesnt start with a mulOp we just return e.      
term' e = mulOp # factor >-> bldOp e #> term' ! return e

-- Does factor function and gives the result and remainder to term'.
-- term "1*2" -> Just(Mul (Num 1) (Num 2), "")
term = factor #> term'
       
-- The same as above but with addOp instead.
expr' e = addOp # term >-> bldOp e #> expr' ! return e
-- term "1+2" -> Just(Add (Num 1) (Num 2), "")
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)

{- 
Implement the function value in Expr. 
The expression value e dictionary should return the value of e if all the variables occur in dictionary and there is no division by zero. 
Otherwise an error should be reported using error.
-}
value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var v) d = case (Dictionary.lookup v d) of
  Nothing -> error ("Expr.value: undefined variable " ++ v)
  Just a -> a
value (Mul e1 e2) d = value e1 d * value e2 d
value (Div e1 e2) d = case value e2 d of
  0 -> error "Expr.value: division by 0"
  _ -> value e1 d `div` value e2 d
value (Add e1 e2) d = value e1 d + value e2 d
value (Sub e1 e2) d = value e1 d - value e2 d

instance Parse Expr where
    parse = expr
    toString = shw 0