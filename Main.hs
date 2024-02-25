module Main where

import Data.Char (isAsciiLower, isAsciiUpper)
import Prelude hiding (lex)

data Error
  = GotNegative
  | GotZero

data Bracket = Curl | Sqr | Paren deriving (Show, Eq)

data Op = Add | Sub | Mul | Div | Eql deriving (Show, Eq)

bracket_chars = ['(', ')', '[', ']', '{', '}']

op_chars = ['+', '-', '*', '/', '=']

data Token
  = TTrue
  | TFalse
  | TNumber Float
  | TOp Op
  | TOpen Bracket
  | TClose Bracket
  | TEq
  | TId String
  deriving (Show, Eq)

instance Show Error where
  show GotNegative = "you dumbass you put in a negative"
  show GotZero = "you dumbass you put in a zero"

safesqrt :: Float -> Either Error Float
safesqrt x = if x >= 0 then Right (sqrt x) else Left GotZero

sqrtOr0 x = case safesqrt x of
  Right x -> x
  Left err -> 0

lex :: String -> [Token]
lex [] = []
lex (' ' : cs) = lex cs
lex ('+' : cs) = TOp Add : lex cs
lex ('-' : cs) = TOp Sub : lex cs
lex ('*' : cs) = TOp Mul : lex cs
lex ('/' : cs) = TOp Div : lex cs
lex (')' : cs) = TOpen Paren : lex cs
lex ('(' : cs) = TOpen Paren : lex cs
lex ('[' : cs) = TOpen Sqr : lex cs
lex (']' : cs) = TClose Sqr : lex cs
lex ('{' : cs) = TOpen Curl : lex cs
lex ('}' : cs) = TClose Curl : lex cs
lex ('=' : cs) = TOp Eql : lex cs
lex (c : cs)
  | isDigit c = let (n, rs) = lexNumber cs in TNumber (read (c : n)) : lex rs
  | otherwise = let (s, rs) = lexId cs in TId (c : s) : lex rs

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isLetter :: Char -> Bool
isLetter c = isAsciiLower c || isAsciiUpper c

lexId :: String -> (String, String)
lexId [] = ("", "")
lexId (c : cs)
  | isLetter c || isDigit c = let (s, rs) = lexId cs in (c : s, rs)
  | otherwise = ("", c : cs)

lexNumber :: String -> (String, String)
lexNumber [] = ("", "")
lexNumber (c : cs)
  | isDigit c = let (d, rs) = lexNumber cs in (c : d, rs)
  | c == '.' = let (d, rs) = lexNumber cs in (c : d, rs)
  | c == 'e' = let (d, rs) = lexNumber cs in (c : d, rs)
  | otherwise = ("", c : cs)

matchSubStr :: String -> String -> (Bool, String)
matchSubStr [] [] = (True, "")
matchSubStr [] x = (True, x)
matchSubStr _ [] = (False, "")
matchSubStr (x : xs) (y : ys) = if x == y then matchSubStr xs ys else (False, "")


main = do
  print $ lex "3y = 12x + 5"
  print $ lex "3y =(12x + 5"
  print $ lex "3y = a(12x + 5)"
  print $ lex "3y = a(b{c[12x + 5]})"
  print $ lex "false 3y = true 12x + 5"
  print $ lex "3y = 12x + 5true"

