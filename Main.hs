module Main where

import Data.Char (isAsciiLower, isAsciiUpper)
import Prelude hiding (lex)

data Error
  = GotNegative
  | GotZero
  | MismatchedBracket [Bracket] [Token]
  | UnclosedBracket [Bracket] [Token]
  | UnfinishedTokenStream
  | BadToken [Token]
  | BadExpr [Expr]
  | BadOp  BinOp
  | EmptyExpr

data Bracket = Curl | Sqr | Paren deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div | Eql deriving (Show, Eq)

bracket_chars = ['(', ')', '[', ']', '{', '}']

op_chars = ['+', '-', '*', '/', '^', '=']

data Token
  = TNumber Float
  | TOp BinOp
  | TOpen Bracket
  | TClose Bracket
  | TEq
  | TId String
  deriving (Show, Eq)

instance Show Error where
  show GotNegative = "you dumbass you put in a negative"
  show GotZero = "you dumbass you put in a zero"
  show (MismatchedBracket stack tokens) = "You dumbass you mismatched a bracket.\nstack: " ++ show stack ++ "\ntokens: " ++ show tokens
  show (UnclosedBracket stack tokens) = "You dumbass you forgot a closing bracket.\nstack: " ++ show stack ++ "\ntokens: " ++ show tokens
  show (BadToken tokens) = "You dumbass you put in a bad token.\ntokens: " ++ show tokens
  show (BadExpr expr) = "You dumbass you put a bad expression.\nexpr" ++ show expr
  show (BadOp op) = "You dumbass you put a bad operator.\noperator: " ++ show op
  

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
lex ('(' : cs) = TOpen Paren : lex cs
lex (')' : cs) = TClose Paren : lex cs
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

data TokTree = TLeaf Token | TNode Bracket [TokTree] deriving (Show, Eq)

tokSubTree :: TokTree -> [TokTree]
tokSubTree (TNode _ ts) = ts

parseTokTree :: [Token] -> Either Error [TokTree]
parseTokTree tokens = case parseInner tokens [] of
  Right ([], t) -> Right t
  Right (left, _) -> Left UnfinishedTokenStream
  Left err -> Left err

-- | Takes in a token stream, and a stack of brackets encountered so far.
-- | Searches from start of token stream up until the next closing bracket.
-- | Returns a tuple of the remaining tokens after the closing bracket, and the TokTree stream before.
parseInner :: [Token] -> [Bracket] -> Either Error ([Token], [TokTree])
parseInner [] [] = Right ([], [])
parseInner [] stack = Left $ UnclosedBracket stack []
parseInner (TClose x : xs) (y : ys)
  | x /= y = Left $ MismatchedBracket (y : ys) (TClose x : xs)
  | otherwise = Right (xs, [])
parseInner (TClose x : xs) [] = Left $ UnclosedBracket [] (TClose x : xs)
parseInner (TOpen x : xs) stack = do  -- don't need cases since errors are infective
  (tokAfterInner, inner) <- parseInner xs (x : stack)  -- parse inner for bracket x
  (tokAfterRest, rest) <- parseInner tokAfterInner stack -- parse tokens after x
  pure (tokAfterRest, TNode x inner : rest) -- put tuple of results into Either
parseInner (x : xs) stack = do
  (restTokens, rest) <- parseInner xs stack
  pure (restTokens, TLeaf x : rest)

data Literal = BoolL Bool | RL Float deriving Show

data Expr = LitE Literal | VarE String | BinOpE BinOp Expr Expr deriving Show


--parseExpr :: [TokTree] -> Either Error Expr
--parseExpr tokTrees = shuntingYard tokTrees [] []

opPresidence :: BinOp -> Int
opPresidence Add = 1
opPresidence Sub = 1
opPresidence Mul = 2
opPresidence Div = 2
opPresidence Eql = 0

opLeftAssoc :: BinOp -> Bool
opLeftAssoc op
  | op == Eql = False
  | otherwise = True

-- shuntingYard :: [TokTree] -> [Expr] -> [BinOp] -> Either Error Expr


-- | Takes in an expresssion stack operator stack and a maybe BinOp of the last operator hit.
-- | Returns the new Expression Stack and the new Operator Stack.
combineExprs :: [Expr] -> [BinOp] -> Maybe BinOp -> Either Error ([Expr], [BinOp])
combineExprs [] [] _ = Left EmptyExpr  -- if both stacks are empty, and there is an op
combineExprs [] stack _ = Left $ BadOp $ head stack -- If there are not enough expressions on the stack
combineExprs exprs [] op = case op of
  Nothing -> case length exprs == 1 of
    True -> Right (exprs, [])
    False -> Left $ BadExpr $ exprs
  Just op -> Right (exprs, [op])
combineExprs (exprL : exprR : rest) opStack maybeOp = case maybeOp of  -- if there are enought expression on the stack
  Just op1 -> case popOp op1 $ head opStack of
    True -> combineExprs ((BinOpE (head opStack) exprL exprR) : rest) (tail opStack) $ Just op1  -- If op1 has presidence over op2, combine the two expressions and put the new expression on the stack
    False -> Right ((exprL : exprR : rest), (op1 : opStack))
  Nothing -> do combineExprs ((BinOpE (head opStack) exprL exprR) : rest) (tail opStack) Nothing
    


-- | Takes an operator op1 and the operator from the top of the stack op2
popOp :: BinOp -> BinOp -> Bool
popOp op1 op2
  | op2p > op1p = True
  | op2p == op2p && opLeftAssoc op1 = True
  | otherwise = False
  where
    op1p = opPresidence op1
    op2p = opPresidence op2

main = do
  print $ lex "3y = 12x + 5"
  print $ lex "3y =(12x + 5"
  print $ lex "3y = a(12x + 5)"
  print $ lex "3y = a(b{c[12x + 5]})"
  print $ lex "false 3y = true 12x + 5"
  print $ lex "3y = 12x + 5true"

  print "TokTree"
  print $ parseTokTree $ lex "(x)"
  print $ parseTokTree $ lex "(x + [x + {x + 1}])"
  print $ parseTokTree $ lex "("
  print $ parseTokTree $ lex ")"
  print $ parseTokTree $ lex "([{}])"

  print "Combine"
  let ct1 = combineExprs [LitE (RL 1), LitE (RL 1)] [Mul] $ Just Add
  print ct1
