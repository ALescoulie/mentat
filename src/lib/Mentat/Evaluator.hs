module Mentat.Evaluator where

import qualified Data.Map.Strict as HM
import Prelude hiding (lex)

import Mentat.ParseTypes
import Mentat.ProgramTypes

applyBinOp :: BinOp -> Literal -> Literal -> Either Error Literal
applyBinOp Add (RL l) (RL r) = Right $ RL (l + r)
applyBinOp Sub (RL l) (RL r) = Right $ RL (l - r)
applyBinOp Mul (RL l) (RL r) = Right $ RL (l * r)
applyBinOp Div (RL x) (RL y) = Right (RL (x / y))
applyBinOp Exp (RL l) (RL r) = Right $ RL (l ** r)
applyBinOp (Comp Eql) (RL x) (RL y) = Right (BoolL (x == y))
applyBinOp (Comp Eql) (BoolL x) (BoolL y) = Right (BoolL (x == y))
applyBinOp (Comp NEq) (RL x) (RL y) = Right (BoolL (x /= y))
applyBinOp (Comp NEq) (BoolL x) (BoolL y) = Right (BoolL (x /= y))
applyBinOp (Comp GEq) (RL x) (RL y) = Right (BoolL (x >= y))
applyBinOp (Comp G) (RL x) (RL y) = Right (BoolL (x > y))
applyBinOp (Comp LEq) (RL x) (RL y) = Right (BoolL (x <= y))
applyBinOp (Comp L) (RL x) (RL y) = Right (BoolL (x < y))
applyBinOp op x y = Left $ LitBinOpError op x y

applyUniOP :: UniOp -> Literal -> Either Error Literal
applyUniOP Neg (RL n) = Right $ RL $ -n
applyUniOP Not (BoolL b) = Right $ BoolL $ not b
applyUniOP Abs (RL n) = Right $ RL $ abs n
applyUniOP Sin (RL n) = Right $ RL $ sin n
applyUniOP Cos (RL n) = Right $ RL $ cos n
applyUniOP Tan (RL n) = Right $ RL $ tan n
applyUniOP Sec (RL n) = Right $ RL $ 1 /  (cos n)
applyUniOP Csc (RL n) = Right $ RL $ 1 / (sin n)
applyUniOP Ctan (RL n) = Right $ RL $ 1 / (tan n)

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
      rest <- evalExpr n vars fxns $ gas - 1
      Right rest
evalExpr (UniOpE op expr) vars fxns gas = do
  l1 <- evalExpr expr vars fxns gas
  result <- applyUniOP op l1
  Right result
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
