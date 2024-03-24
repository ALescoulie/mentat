module Mentat.Lexer where

import Mentat.ParseTypes (BinOp(..), CompOp(..), Bracket(..), Token(..), UniOp(..))
import Prelude hiding (lex)
import qualified Data.Map.Strict as HM
import Data.Maybe (isJust, fromMaybe)


lex :: String -> [Token]
lex [] = []
lex (' ':cs) = lex cs
lex ('+':cs) = TOp Add : lex cs
lex ('-':cs) = TNeg : lex cs
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
lex ('=':cs) = (TOp $ Comp Eql) : lex cs
lex ('<':'=':cs) = (TOp $ Comp LEq) : lex cs
lex ('>':'=':cs) = (TOp $ Comp GEq) : lex cs
lex ('<':cs) = (TOp $ Comp L) : lex cs
lex ('>':cs) = (TOp $ Comp G) : lex cs
lex ('!':'=':cs) = (TOp $ Comp NEq) : lex cs
lex (':':'=':cs) = TAsgn : lex cs
lex (c:cs) = case isDigit c of
  True -> do
    let (n, rs) = lexNumber cs
     in TNumber (read (c : n)) : lex rs
  False -> do
    let maybeMultiChar = lexMultiCharToken (c:cs)
    case maybeMultiChar of
      Just (token, rest) -> token : lex rest
      Nothing -> do
        let (id, rest) = lexId (c:cs)
        TId id : lex rest

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'


isSeperator :: String -> Bool
isSeperator "" = True
isSeperator (',':_) = True
isSeperator ('+':_) = True
isSeperator ('-':_) = True
isSeperator ('*':_) = True
isSeperator ('/':_) = True
isSeperator ('^':_) = True
isSeperator ('(':_) = True
isSeperator (')':_) = True
isSeperator ('[':_) = True
isSeperator (']':_) = True
isSeperator ('{':_) = True
isSeperator ('}':_) = True
isSeperator (',':_) = True
isSeperator (';':_) = True
isSeperator ('-':_) = True
isSeperator ('!':_) = True
isSeperator ('>':_) = True
isSeperator ('<':_) = True
isSeperator (':':_) = True
isSeperator (' ':_) = True
isSeperator _ = False


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

-- | Matches first in second string returns Maybe and rest of the string on sucess and Nothing on failure
matchSubStr :: String -> String -> Maybe String
matchSubStr [] [] = Just ""
matchSubStr [] x = Just x
matchSubStr _ [] = Nothing
matchSubStr (x:xs) (y:ys) =
  if x == y
    then matchSubStr xs ys
    else Nothing

multiCharTokens = HM.fromList
  [ ("false", TFalse)
  , ("true", TTrue)
  , ("and", TOp And)
  , ("or", TOp Or)
  , ("xor", TOp Xor)
  , ("not", TUOp Not)
  , ("sin", TUOp Sin)
  , ("cos", TUOp Cos)
  , ("tan", TUOp Tan)
  , ("sec", TUOp Sec)
  , ("csc", TUOp Csc)
  , ("ctan", TUOp Ctan)
  ]

lexMultiCharToken :: String -> Maybe (Token, String)
lexMultiCharToken [] = Nothing
lexMultiCharToken s = do
  let maybeMatches = map (\x -> (x, matchSubStr x s)) $ HM.keys multiCharTokens
  let matchPairs = filter (\(needle, maybeMatch) -> isJust maybeMatch) maybeMatches
  case matchPairs of
    [] -> Nothing
    matches -> do
      let (needle, match) = head matches
      case match of
        Nothing -> Nothing
        Just rest -> do
          token <- HM.lookup needle multiCharTokens
          if isSeperator rest then
            Just (token, rest)
          else
            Nothing
  
