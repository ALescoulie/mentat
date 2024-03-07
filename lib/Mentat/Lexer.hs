module Mentat.Lexer where

import Mentat.ParseTypes (BinOp(..), Bracket(..), Token(..))
import Prelude hiding (lex)

lex :: String -> [Token]
lex [] = []
lex (' ':cs) = lex cs
lex ('+':cs) = TOp Add : lex cs
lex ('-':cs) = TOp Sub : lex cs
lex ('*':cs) = TOp Mul : lex cs
lex ('/':cs) = TOp Div : lex cs
lex ('^':cs) = TOp Exp : lex cs
lex ('(':cs) = TOpen Paren : lex cs
lex (')':cs) = TClose Paren : lex cs
lex ('[':cs) = TOpen Sqr : lex cs
lex (']':cs) = TClose Sqr : lex cs
lex ('{':cs) = TOpen Curl : lex cs
lex ('}':cs) = TClose Curl : lex cs
lex (',':cs) = TSep : lex cs
lex ('=':cs) = TOp Eql : lex cs
lex ('<':'=':cs) = TOp LEq : lex cs
lex ('>':'=':cs) = TOp GEq : lex cs
lex ('<':cs) = TOp L : lex cs
lex ('>':cs) = TOp G : lex cs
lex ('!':'=':cs) = TOp NEq : lex cs
lex (':':'=':cs) = TAsgn : lex cs
lex ('f':'a':'l':'s':'e':cs) =
  case cs of
    [] -> [TFalse]
    (' ':rest) -> TFalse : lex rest
    (cNext:rest) ->
      let (s, rs) = lexId ("false" ++ cNext : rest)
       in TId s : lex rs
lex ('t':'r':'u':'e':cs) =
  case cs of
    [] -> [TTrue]
    (' ':rest) -> TTrue : lex rest
    (cNext:rest) ->
      let (s, rs) = lexId ("true" ++ cNext : rest)
       in TId s : lex rs
lex (c:cs)
  | isDigit c =
    let (n, rs) = lexNumber cs
     in TNumber (read (c : n)) : lex rs
  | otherwise =
    let (s, rs) = lexId cs
     in TId (c : s) : lex rs

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

lexId :: String -> (String, String)
lexId [] = ("", "")
lexId (':':cs) = ("", ':' : cs)
lexId ('.':cs) = ("", '.' : cs)
lexId (',':cs) = ("", ',' : cs)
lexId (' ':cs) = ("", ' ' : cs)
lexId ('+':cs) = ("", '+' : cs)
lexId ('-':cs) = ("", '-' : cs)
lexId ('*':cs) = ("", '*' : cs)
lexId ('/':cs) = ("", '/' : cs)
lexId ('^':cs) = ("", '^' : cs)
lexId ('(':cs) = ("", '(' : cs)
lexId (')':cs) = ("", ')' : cs)
lexId ('[':cs) = ("", '[' : cs)
lexId (']':cs) = ("", ']' : cs)
lexId ('{':cs) = ("", '{' : cs)
lexId ('}':cs) = ("", '}' : cs)
lexId ('=':cs) = ("", '=' : cs)
lexId ('<':'=':cs) = ("", "<=" ++ cs)
lexId ('>':'=':cs) = ("", ">=" ++ cs)
lexId ('<':cs) = ("", '<' : cs)
lexId ('>':cs) = ("", '>' : cs)
lexId ('!':'=':cs) = ("", "!=" ++ cs)
lexId (c:cs) = do
  let (s, rs) = lexId cs
  (c : s, rs)

-- | Takes in a string reperesnting a the statment and the rest of the string
lexNumber :: String -> (String, String)
lexNumber [] = ("", "")
lexNumber (c:cs)
  | isDigit c =
    let (d, rs) = lexNumber cs
     in (c : d, rs)
  | c == '.' =
    let (d, rs) = lexNumber cs
     in (c : d, rs)
  | c == 'e' =
    let (d, rs) = lexNumber cs
     in (c : d, rs)
  | otherwise = do
    let tok = lex [c]
    case tok of
      (TId _:_) -> ([], '*' : c : cs)
      _ -> ([], c : cs)

-- | Matches first in second string
matchSubStr :: String -> String -> (Bool, String)
matchSubStr [] [] = (True, "")
matchSubStr [] x = (True, x)
matchSubStr _ [] = (False, "")
matchSubStr (x:xs) (y:ys) =
  if x == y
    then matchSubStr xs ys
    else (False, "")
