-- contains a number of derived parsers and parser operators.

module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

-- Iterates a parser as long as it succeeds.
iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b
m -# n = error "-# not implemented"

(#-) :: Parser a -> Parser b -> Parser a
m #- n = error "#- not implemented"

-- spaces accepts any number of whitespace characters as defined by the Prelude function isSpace.
spaces :: Parser String
spaces =  iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

-- letter is a parser for a letter as defined by the Prelude function isAlpha (True if the character is an alphabetic character).
-- Applies the char function to the string and checks if it is alphabetic character uing the prelud function.
letter :: Parser Char
letter =  (char ? isAlpha)

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars n =  error "chars not implemented"

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String
require w  = error "require not implemented"

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

