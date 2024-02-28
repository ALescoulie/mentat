{-# LANGUAGE LambdaCase #-}

module Mentat.SyntaxParser where

import Prelude hiding ( lex )
import Mentat.ParseTypes
import Mentat.Lexer ( lex )
import Mentat.Tokenizer ( parseTokTree )
import qualified Data.Map.Strict as HM
import Data.Maybe ( mapMaybe )

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

buildOpExpr :: BinOp -> Expr -> Expr -> Expr
buildOpExpr op e1 e2 = if opLeftAssoc op then BinOpE op e1 e2 else BinOpE op e2 e1

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
    True -> combineExprs (buildOpExpr op2 exprL exprR : rest) (rops) $ Just op1 -- If op1 has presidence over op2 combine the two expressions and put the new expression on the stack
    False -> Right ((exprL : exprR : rest), (op1 : op2 : rops))
  Nothing -> do combineExprs (buildOpExpr op2 exprL exprR : rest) rops Nothing
combineExprs _ _ _ = Left EmptyExpr


parseDecl :: [TokTree] -> Either Error Statment
parseDecl [] = Left EmptyExpr
parseDecl (TLeaf (TId i) : TLeaf TAsgn : rest) = do
  expr <- parseExpr rest
  Right $ Declaration i expr
parseDecl other = Left $ BadDecl other

parseAsgn _ = Left EmptyExpr



getProgramStatments :: Program -> [Statment]
getProgramStatments (Program s) = s

parseProgram :: [String] -> Either Error Program
parseProgram [] = Right (Program [])
parseProgram (s : ss) = do
  let tokens = lex s
  tokTree <- parseTokTree tokens
  let decl = parseDecl tokTree
  rest <- parseProgram ss
  let restStatments = getProgramStatments rest
  case decl of
    Left _ -> do
      expr <- parseExpr tokTree
      Right $ Program (Constraint expr : restStatments)
    Right decl -> do
      rest <- parseProgram ss
      Right $ Program (decl : restStatments)

repeatItems :: Eq a => [a] -> [a]
repeatItems [] = []
repeatItems (x : xs) = filter (== x) xs ++ repeatItems xs

parseVariables :: Program -> Either Error (HM.Map String Expr)
parseVariables (Program []) = Right HM.empty
parseVariables (Program smtList) = do
  let decl =
        mapMaybe
          ( \case
              Declaration n e -> Just (n, e)
              _ -> Nothing
          )
          smtList
  let vars = map fst decl
  let expr = map snd decl
  let repeats = repeatItems vars
  if null repeats then Right (HM.fromList decl) else Left $ DuplicateVars repeats

