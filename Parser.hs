-- contains a number of derived parsers and parser operators.
module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail, iterate)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n") 

-- Iterates a parser as long as it succeeds.
iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

-- Applies the parser m to the input string i times with the result in a list with i elements.
iterate :: Parser a -> Int -> Parser [a]
iterate m 0 = return []
iterate m i = m # iterate m (i-1) >-> cons

cons(a, b) = a:b

-- The parser m -# n accepts the same input as m # n, but returns just the result from the n parser. 
-- The function should be declared as a left associative infix operator with precedence 7
-- (accept "read" -# word) "read count;" -> Just ("count", ";")
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

-- spaces accepts any number of whitespace characters as defined by the Prelude function isSpace.
spaces :: Parser String
spaces =  iter (char ? isSpace)

-- We define token which for a given parser will return a parser which will remove any whitespace after the accepted string.
token :: Parser a -> Parser a
token m = m #- spaces

-- letter is a parser for a letter as defined by the Prelude function isAlpha (True if the character is an alphabetic character).
-- Applies the char function to the string and checks if it is alphabetic character uing the prelud function.
letter :: Parser Char
letter =  (char ? isAlpha)

-- word "count := count+1" -> Just("count", ":= count+1")
word :: Parser String
word = token (letter # iter letter >-> cons)

-- The parser chars n accepts n characters.
chars :: Int -> Parser String
chars n =  iterate char n

{-
accept "while" "while x do x:=x-1" -> Just("while", "x do x:=x-1")
accept "if" "while x do x:=x-1" -> Nothing
-}
accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

-- The parser require w accepts the same string input as accept w but reports the missing string using err in case of failure.
require :: String -> Parser String
require w  = (accept w ! err w)

-- A parser accepting a given character is easily defined.
-- The predicate decides if the given character is the the same as the one accepted by char.
lit :: Char -> Parser Char
lit c = token char ? (==c)

-- Accepts one digit
-- digit "123" -> Just(’1’,"23")
digit :: Parser Char 
digit = char ? isDigit 

-- We may use it to define a parser which accepts a digit and returns an Int using digitToInt from the Prelude.
-- digitVal "123" -> Just(1, "23")
digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n

-- number "123 abc" -> Just(123, "abc")
number :: Parser Integer
number = token (digitVal #> number')