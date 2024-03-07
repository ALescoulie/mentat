{-# LANGUAGE LambdaCase #-}

module Mentat.SyntaxParser where

import Data.Foldable (toList)
import Data.List (intersect)
import qualified Data.Map.Strict as HM
import Data.Maybe (mapMaybe)
import Mentat.Lexer (lex)
import Mentat.ParseTypes
import Mentat.Tokenizer (parseTokTree)
import Prelude hiding (lex)

-- | Parses TokTree list into expression
parseExpr :: [TokTree] -> Either Error Expr
parseExpr tokens = shuntingYard tokens [] []

-- | helper for parseExpr
shuntingYard :: [TokTree] -> [Expr] -> [BinOp] -> Either Error Expr
shuntingYard [] exprs ops
  -- if there are no more tokens
 = do
  (mergedExprs, mergedOps) <- combineExprs exprs ops Nothing -- merge the remaing exprs and ops
  case length mergedExprs == 1 && length mergedOps == 0 -- if there are extra exprs or ops throw error
        of
    True -> Right $ head mergedExprs -- if no extras return expr
    False -> Left EmptyExpr -- if there are extras throw error
shuntingYard (toT:restT) exprs ops =
  case toT of
    TLeaf (TNumber n) -> shuntingYard restT (LitE (RL n) : exprs) ops
    TLeaf (TId i) -> shuntingYard restT (VarE i : exprs) ops
    TLeaf (TOp op) -> do
      (mergedExprs, mergedOps) <- combineExprs exprs ops $ Just op
      shuntingYard restT mergedExprs mergedOps
    TNode _ innerTok -> do
      innerExpr <- shuntingYard innerTok [] []
      shuntingYard restT (innerExpr : exprs) ops
    TFxn name args -- if you encounter a fxn
     -> do
      argExprs <- mapM (\x -> shuntingYard x [] []) args
      shuntingYard restT (FxnE name argExprs : exprs) ops
    _ -> Left EmptyExpr

-- | Helper for shuntingYard
buildOpExpr :: BinOp -> Expr -> Expr -> Expr
buildOpExpr op e1 e2 =
  if opLeftAssoc op
    then BinOpE op e1 e2
    else BinOpE op e2 e1

-- | Helper for shuntingYard
-- | Takes in an expresssion stack operator stack and a maybe BinOp of the last operator hit.
-- | Returns the new Expression Stack and the new Operator Stack.
combineExprs ::
     [Expr] -> [BinOp] -> Maybe BinOp -> Either Error ([Expr], [BinOp])
combineExprs [] [] _ = Left EmptyExpr -- if both stacks are empty, and there is an op
combineExprs [] (op:rOps) _ = Left $ BadOp $ op -- If there are not enough expressions on the stack
combineExprs exprs [] op =
  case op -- If there are exprs left and no ops on the stack
        of
    Nothing ->
      case length exprs == 1 -- If op is Nothing, means all tokens are parsed
            of
        True -> Right (exprs, []) -- if tokens are constructed into a single tree return it
        False -> Left $ BadExpr $ exprs -- if not return error
    Just op -> Right (exprs, [op]) -- if op add to stack and return
combineExprs (exprL:exprR:rest) (op2:rops) maybeOp =
  case maybeOp -- if there are enought expression on the stack
        of
    Just op1 ->
      case popOp op1 op2 of
        True ->
          combineExprs (buildOpExpr op2 exprL exprR : rest) (rops) $ Just op1 -- If op1 has presidence over op2 combine the two expressions and put the new expression on the stack
        False -> Right ((exprL : exprR : rest), (op1 : op2 : rops))
    Nothing -> do
      combineExprs (buildOpExpr op2 exprL exprR : rest) rops Nothing
combineExprs _ _ _ = Left EmptyExpr

-- | Parses declerations from TokTree lists
parseDecl :: [TokTree] -> Either Error Statment
parseDecl [] = Left EmptyExpr
parseDecl (TLeaf (TId i):TLeaf TAsgn:rest) = do
  expr <- parseExpr rest
  Right $ Declaration i expr
parseDecl other = Left $ BadDecl other

-- | Parse Function
parseFxn :: [TokTree] -> Either Error Statment
parseFxn [] = Left EmptyExpr
parseFxn ((TFxn name args):TLeaf TAsgn:rest) = do
  fxnArgs <- parseFxnArgs args
  fxnExpr <- parseExpr rest
  Right $ Fxn $ Function name fxnArgs fxnExpr
parseFxn tokTrees = Left EmptyExpr

parseFxnArgs :: [[TokTree]] -> Either Error [String]
parseFxnArgs [] = Right []
parseFxnArgs ([TLeaf (TId arg)]:restToks) = do
  restArgs <- parseFxnArgs restToks
  Right $ arg : restArgs
parseFxnArgs ([_]:restArgs) = Left UnfinishedTokenStream -- TODO add function parsing errors

-- | return the statments of a program
getProgramStatments :: Program -> [Statment]
getProgramStatments (Program s) = s

-- | returns the statments that are expressions
filterExprs :: [Statment] -> [Expr]
filterExprs [] = []
filterExprs (Constraint expr:rest) = expr : filterExprs rest
filterExprs (_:rest) = filterExprs rest

getVarNames :: [Statment] -> [String]
getVarNames [] = []
getVarNames (Declaration name _:rest) = name : getVarNames rest
getVarNames (_:xs) = getVarNames xs

getFxnNames :: [Statment] -> [String]
getFxnNames [] = []
getFxnNames (Fxn (Function name _ _):rest) = name : getFxnNames rest
getFxnNames (_:xs) = getFxnNames xs

-- | parses a list of strings into a program
-- | is a helper for parse program
parseProgram' :: [String] -> Either Error Program
parseProgram' [] = Right (Program [])
parseProgram' (s:ss) = do
  let tokens = lex s
  tokTree <- parseTokTree tokens
  let maybeDecl = parseDecl tokTree
  rest <- parseProgram' ss
  let restStatments = getProgramStatments rest
  case maybeDecl of
    Left _ -> do
      let maybeFunction = parseFxn tokTree
      case maybeFunction of
        Left _ -> do
          expr <- parseExpr tokTree
          Right $ Program (Constraint expr : restStatments)
        Right fxn -> Right $ Program (fxn : restStatments)
    Right decl -> Right $ Program (decl : restStatments)

-- | Parses and validates a list of strings into a program
parseProgram :: [String] -> Either Error Program
parseProgram [] = Right (Program [])
parseProgram pg = do
  programStats <- parseProgram' pg
  let declNames = getVarNames $ getProgramStatments programStats
  let fxnNames = getFxnNames $ getProgramStatments programStats
  let repeatNames = repeatItems $ declNames ++ fxnNames
  if null repeatNames
    then Right programStats
    else Left $ DuplicateVars repeatNames

-- | accpets a lilst and returns any repeated items
-- | a helper for varriable parsing
repeatItems :: Eq a => [a] -> [a]
repeatItems [] = []
repeatItems (x:xs) = filter (== x) xs ++ repeatItems xs

parseVariables :: Program -> Either Error (HM.Map String Expr)
parseVariables (Program []) = Right HM.empty
parseVariables (Program smtList) = do
  let decl =
        mapMaybe
          (\case
             Declaration n e -> Just (n, e)
             _ -> Nothing)
          smtList
  let vars = map fst decl
  let repeats = repeatItems vars
  if null repeats
    then Right (HM.fromList decl)
    else Left $ DuplicateVars repeats

parseFxns :: Program -> Either Error (HM.Map String Function)
parseFxns (Program []) = Right HM.empty
parseFxns (Program smtList) = do
  let fxnNames = getFxnNames smtList
  let fxns =
        mapMaybe
          (\case
             Fxn f -> Just f
             _ -> Nothing)
          smtList
  let fxnList = zip fxnNames fxns
  Right (HM.fromList fxnList)

buildExpr :: String -> Either Error Expr
buildExpr [] = Left EmptyExpr
buildExpr str = do
  let tokens = lex str
  tokTree <- parseTokTree tokens
  expr <- parseExpr tokTree
  Right expr
