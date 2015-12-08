{- Test for Parser.hs -}
module TestParser where

import Prelude hiding (return, fail)
import Parser

l1 = letter "abc"   {- Just('a',"bc") -}
l2 = letter "123"   {- Nothing -}
l3 = letter ""      {- Nothing -}

w1 = spaces "abc"  {- Just("","abc") -}
w2 = spaces "  \t abc"  {- Just("  \t ","abc") -}

c1 = chars 2 "abc"          {-  Just ("ab","c")  -}
c2 = chars 0 "ab"          {-  Just ("","ab")  -}
c3 = chars 3 "ab"          {-  Nothing)  -}

r1 = require ":=" ":= 1"     {- Just (":=","1") -}
r2 = require "else" "then"     {- Program error: expecting else near then -}

a4 = (accept "read" -# word) "read count" {-  Just ("count","") -}
