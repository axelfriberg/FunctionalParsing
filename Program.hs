{- 
contains a data type for representing a program, a program parser, a program interpreter, 
and a function for converting the representation to a string.

In the Program module you should represent the program as a Statement list. 
Use the parse function from the Statement module to define the parse function in this module. 
Use the exec function in the Statement module to execute a program.
-}

module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T]
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program p) = concatMap Statement.toString p

exec (Program p) = Statement.exec p Dictionary.empty 