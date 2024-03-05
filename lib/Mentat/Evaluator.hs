module Mentat.Evaluator where

import Prelude hiding ( lex )
import qualified Data.Map.Strict as HM

import Mentat.ParseTypes
import Mentat.Lexer ( lex )
import Mentat.Tokenizer ( parseTokTree )
import Mentat.SyntaxParser ( parseExpr )

applyBinOp :: BinOp -> Literal -> Literal -> Either Error Literal
applyBinOp Add (RL l) (RL r) = Right $ RL (l + r)
applyBinOp Sub (RL l) (RL r) = Right $ RL (l - r)
applyBinOp Mul (RL l) (RL r) = Right $ RL (l * r)
applyBinOp Div (RL x) (RL y) = case y == 0 of
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


evalExpr :: Expr -> HM.Map String Expr -> Either Error Literal
evalExpr (LitE (RL n)) _ = Right (RL n)
evalExpr (LitE (BoolL b)) _ = Right (BoolL b)
evalExpr (VarE i) vars = do
  let val = HM.lookup i vars
  case val of
    Nothing -> Left $ MissingVar i
    Just n -> do
      rest <- evalExpr n vars
      Right rest
evalExpr (BinOpE op e1 e2) vars = do
  l1 <- evalExpr e1 vars
  l2 <- evalExpr e2 vars
  result <- applyBinOp op l1 l2
  Right result

