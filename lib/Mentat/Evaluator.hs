module Mentat.Evaluator where

import qualified Data.Map.Strict as HM
import Prelude hiding (lex)

import Mentat.Lexer (lex)
import Mentat.ParseTypes
import Mentat.SyntaxParser (parseExpr)
import Mentat.Tokenizer (parseTokTree)

applyBinOp :: BinOp -> Literal -> Literal -> Either Error Literal
applyBinOp Add (RL l) (RL r) = Right $ RL (l + r)
applyBinOp Sub (RL l) (RL r) = Right $ RL (l - r)
applyBinOp Mul (RL l) (RL r) = Right $ RL (l * r)
applyBinOp Div (RL x) (RL y) =
  case y == 0 of
    True -> Left $ LitBinOpError Div (RL x) (RL y)
    False -> Right (RL (x / y))
applyBinOp Exp (RL x) (RL y) = Right $ RL (x ** y)
applyBinOp Eql (RL x) (RL y) = Right (BoolL (x == y))
applyBinOp Eql (BoolL x) (BoolL y) = Right (BoolL (x == y))
applyBinOp NEq (RL x) (RL y) = Right (BoolL (x /= y))
applyBinOp NEq (BoolL x) (BoolL y) = Right (BoolL (x /= y))
applyBinOp GEq (RL x) (RL y) = Right (BoolL (x >= y))
applyBinOp G (RL x) (RL y) = Right (BoolL (x > y))
applyBinOp LEq (RL x) (RL y) = Right (BoolL (x <= y))
applyBinOp L (RL x) (RL y) = Right (BoolL (x < y))
applyBinOp op x y = Left $ LitBinOpError op x y

evalExpr ::
     Expr
  -> HM.Map String Expr
  -> HM.Map String Function
  -> Int
  -> Either Error Literal
evalExpr (LitE (RL n)) _ _ _ = Right (RL n)
evalExpr (LitE (BoolL b)) _ _ _ = Right (BoolL b)
evalExpr (VarE i) vars fxns gas = do
  let val = HM.lookup i vars
  case val of
    Nothing -> Left $ MissingVar i
    Just n -> do
      rest <- evalExpr n vars fxns gas
      Right rest
evalExpr (BinOpE op e1 e2) vars fxns gas = do
  l1 <- evalExpr e1 vars fxns gas
  l2 <- evalExpr e2 vars fxns gas
  result <- applyBinOp op l1 l2
  Right result
evalExpr _ _ _ 0 = Left EmptyExpr -- TODO add error for max recursion depth
evalExpr (FxnE name argExprs) vars fxns gas = do
  let fxn = HM.lookup name fxns
  case fxn of
    Nothing -> Left EmptyExpr -- TODO add error for missing function
    Just (Function _ argNames expr) -> do
      case length argExprs == length argNames of
        False -> Left EmptyExpr -- TODO add error for wrong number of args
        True -> do
          let fxnVars = HM.union (HM.fromList $ zip argNames argExprs) vars
          fxnResult <- evalExpr expr fxnVars fxns $ gas - 1
          Right fxnResult
