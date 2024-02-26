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
  | BadOp BinOp
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
parseInner (TOpen x : xs) stack = do
  -- don't need cases since errors are infective
  (tokAfterInner, inner) <- parseInner xs (x : stack) -- parse inner for bracket x
  (tokAfterRest, rest) <- parseInner tokAfterInner stack -- parse tokens after x
  pure (tokAfterRest, TNode x inner : rest) -- put tuple of results into Either
parseInner (x : xs) stack = do
  (restTokens, rest) <- parseInner xs stack
  pure (restTokens, TLeaf x : rest)

data Literal = BoolL Bool | RL Float deriving (Show)

data Expr = LitE Literal | VarE String | BinOpE BinOp Expr Expr deriving (Show)

-- parseExpr :: [TokTree] -> Either Error Expr
-- parseExpr tokTrees = shuntingYard tokTrees [] []

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

parseExpr :: [TokTree] -> Either Error Expr
parseExpr tokens = shuntingYard tokens [] []

shuntingYard :: [TokTree] -> [Expr] -> [BinOp] -> Either Error Expr
shuntingYard [] exprs ops = do
  -- if there are no more tokens
  (mergedExprs, mergedOps) <- combineExprs exprs ops Nothing -- merge the remaing exprs and ops
  case length mergedExprs == 1 && length mergedOps == 0 of -- if there are extra exprs or ops throw error
    True -> Right $ head mergedExprs -- if no extras return expr
    False -> Left EmptyExpr -- if there are extras throw error
shuntingYard (toT : restT) exprs ops = case toT of
  TLeaf (TNumber n) -> shuntingYard restT (LitE (RL n) : exprs) ops
  TLeaf (TId i) -> shuntingYard restT (VarE i : exprs) ops
  TLeaf (TOp op) -> do
    (mergedExprs, mergedOps) <- combineExprs exprs ops $ Just op
    shuntingYard restT mergedExprs mergedOps
  TNode node innerTok -> do
    innerExpr <- shuntingYard innerTok [] []
    shuntingYard restT (innerExpr : exprs) ops
  _ -> Left EmptyExpr

-- | Takes in an expresssion stack operator stack and a maybe BinOp of the last operator hit.
-- | Returns the new Expression Stack and the new Operator Stack.
combineExprs :: [Expr] -> [BinOp] -> Maybe BinOp -> Either Error ([Expr], [BinOp])
combineExprs [] [] _ = Left EmptyExpr -- if both stacks are empty, and there is an op
combineExprs [] (op : rOps) _ = Left $ BadOp $ op -- If there are not enough expressions on the stack
combineExprs exprs [] op = case op of -- If there are exprs left and no ops on the stack
  Nothing -> case length exprs == 1 of -- If op is Nothing, means all tokens are parsed
    True -> Right (exprs, []) -- if tokens are constructed into a single tree return it
    False -> Left $ BadExpr $ exprs -- if not return error
  Just op -> Right (exprs, [op]) -- if op add to stack and return
combineExprs (exprL : exprR : rest) (op2 : rops) maybeOp = case maybeOp of -- if there are enought expression on the stack
  Just op1 -> case popOp op1 op2 of
    True -> combineExprs ((BinOpE (op2) exprL exprR) : rest) (rops) $ Just op1 -- If op1 has presidence over op2, combine the two expressions and put the new expression on the stack
    False -> Right ((exprL : exprR : rest), (op1 : op2 : rops))
  Nothing -> do combineExprs ((BinOpE (op2) exprL exprR) : rest) rops Nothing
combineExprs _ _ _ = Left EmptyExpr

-- | Takes an operator op1 and the operator from the top of the stack op2
popOp :: BinOp -> BinOp -> Bool
popOp op1 op2
  | op2p > op1p = True
  | op1p == op2p && opLeftAssoc op1 = True
  | otherwise = False
  where
    op1p = opPresidence op1
    op2p = opPresidence op2

main = do
  print "Parse into tokens"
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
  let ct1 = combineExprs [LitE (RL 1), LitE (RL 1)] [Add] $ Just Mul
  print ct1

  print "Shunting Yard"
  let pt1 = parseTokTree $ lex "3 + 2"
  print pt1
  case pt1 of
    Right tokTrees -> print $ parseExpr tokTrees
    Left err -> print err

  let pt2 = parseTokTree $ lex "3 + 2 * 4"
  print pt2
  case pt2 of
    Right tokTrees -> print $ parseExpr tokTrees
    Left err -> print err

  let pt3 = parseTokTree $ lex "3 + 2 * (4 + 5)"
  print pt3
  case pt3 of
    Right tokTrees -> print $ parseExpr tokTrees
    Left err -> print err
