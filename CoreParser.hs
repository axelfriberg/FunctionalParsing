{- defines the Parser type and implements the three elementary parsers, char, return and fail, 
and the basic parser operators #, !, ?, #>, and >->, described in Lennart Andersson's description (and during the lecture).
The class Parse with signatures for parse, toString, and fromString with an implementation for the last one is introduced.
The representation of the Parser type is visible outside the module, but this visibilty should not be exploited.-}

module CoreParser(Parser, char, return, fail, (#), (!), (?), (#>), (>->),
                  Parse, parse, toString, fromString) where
import Prelude hiding (return, fail)
infixl 3 ! 
infixl 7 ?
infixl 6 #
infixl 5 >->
infixl 4 #>

class Parse a where
    parse :: Parser a
    fromString :: String -> a
    fromString cs =
        case parse cs of
               Just(s, []) -> s
               Just(s, cs) -> error ("garbage '"++cs++"'")
               Nothing -> error "Nothing"
    toString :: a -> String

type Parser a = String -> Maybe (a, String)

-- Seperates the first char in a string.
char :: Parser Char
char []= Nothing
char (c:cs) = Just (c, cs)

-- Will always succeed without inspecting the input string.
return :: a -> Parser a
return a cs = Just (a, cs)

-- Will always fail without inspecting the input string. 
fail ::  Parser a 
fail cs = Nothing

-- If m and n are parsers of the same type then this will be a parser which first applies m to the input string which will be the results unless m fails. 
-- If it fails, n is applied to the input.
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs = case m cs of
             Nothing -> n cs 
             mcs -> mcs

-- If m is a parser and p is a predicate, then this parser will apply m to the input string and test if the result satisfies p.
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs = 
    case m cs of
    Nothing -> Nothing
    Just(r, s) -> if p r then Just(r, s) else Nothing

-- This parser applies two parsers in sequence where the remainder string from the first one is fed into the other, the results of the two parsers are combined into a pair.
(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs = 
    case m cs of
    Nothing -> Nothing
    Just(a, cs') -> 
        case n cs' of
        Nothing -> Nothing
        Just(b, cs'') -> Just((a, b), cs'')

-- The right operand, b, is the function defining the transformation. The result is a new parser. Could for instance be used to transform digit to int.
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> b) cs = 
    case m cs of
    Just(a, cs') -> Just(b a, cs')
    Nothing -> Nothing

-- Provides the means to make the result from one parser available to another.
-- After applying the first parser to cs both the result and the raminder cs are given to the second operand.
(#>) :: Parser a -> (a -> Parser b) -> Parser b 
(p #> k) cs = 
    case p cs of
    Nothing -> Nothing
    Just(a, cs') -> k a cs'
