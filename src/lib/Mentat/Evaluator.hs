{-# LANGUAGE LambdaCase #-}

module Mentat.Evaluator where

import qualified Data.Map.Strict as HM
import Prelude hiding (lex)

import Data.List ( (!?) )
import GHC.Num.Integer (integerEncodeDouble, integerToInt, integerFromInt)

import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.ParseTypes (MtConvert(makeMtType))


applyBinOp :: BinOp -> MtType -> MtType -> Either Error MtType
applyBinOp Add l r = do
  case (l, r) of
    (PrimType(Numeric (MtInt ln)), PrimType(Numeric (MtInt rn))) -> Right $ makeMtType $ ln + rn
    (PrimType (Numeric (Num ln)), PrimType (Numeric (Num rn))) -> Right $ makeMtType $ ln + rn
    (PrimType(Numeric (MtInt ln)), PrimType (Numeric (Num rn))) -> Right $ makeMtType $ (integerEncodeDouble ln 1) + rn
    (PrimType (Numeric (Num ln)), PrimType(Numeric (MtInt rn))) -> Right $ makeMtType $ ln + (integerEncodeDouble rn 1)
    (t1, t2) -> Left $ TypeError "+" [t1, t2]
applyBinOp Sub l r = do
  case (l, r) of
    (PrimType(Numeric (MtInt ln)), PrimType(Numeric (MtInt rn))) -> Right $ makeMtType $ ln - rn
    (PrimType (Numeric (Num ln)), PrimType (Numeric (Num rn))) -> Right $ makeMtType $ ln - rn
    (PrimType(Numeric (MtInt ln)), PrimType (Numeric (Num rn))) -> Right $ makeMtType $ (integerEncodeDouble ln 1) - rn
    (PrimType (Numeric (Num ln)), PrimType(Numeric (MtInt rn))) -> Right $ makeMtType $ ln - (integerEncodeDouble rn 1)
    (t1, t2) -> Left $ TypeError "-" [t1, t2]
applyBinOp Mul l r = do
  case (l, r) of
    (PrimType(Numeric (MtInt ln)), PrimType(Numeric (MtInt rn))) -> Right $ makeMtType $ ln * rn
    (PrimType (Numeric (Num ln)), PrimType (Numeric (Num rn))) -> Right $ makeMtType $ ln * rn
    (PrimType(Numeric (MtInt ln)), PrimType (Numeric (Num rn))) -> Right $ makeMtType $ (integerEncodeDouble ln 1) * rn
    (PrimType (Numeric (Num ln)), PrimType(Numeric (MtInt rn))) -> Right $ makeMtType $ ln * (integerEncodeDouble rn 1)
    (t1, t2) -> Left $ TypeError "*" [t1, t2]
--applyBinOp Div l r = do
--  case (l, r) of
--    (PrimType(Numeric (MtInt ln)), PrimType(Numeric (MtInt rn))) -> Right $ makeMtType $ ln / rn
--    (PrimType (Numeric (Num ln)), PrimType (Numeric (Num rn))) -> Right $ makeMtType $ ln / rn
--    (PrimType(Numeric (MtInt ln)), PrimType (Numeric (Num rn))) -> Right $ makeMtType $ (integerEncodeDouble ln 1) / rn
--    (PrimType (Numeric (Num ln)), PrimType(Numeric (MtInt rn))) -> Right $ makeMtType $ ln / (integerEncodeDouble rn 1)
--    (t1, t2) -> Left $ TypeError "/" [t1, t2]
--applyBinOp Exp l r = do
--  case (l, r) of
--    (PrimType(Numeric (MtInt ln)), PrimType(Numeric (MtInt rn))) -> Right $ PrimType $ Numeric $ MtInt $ 
--    (PrimType (Numeric (Num ln)), PrimType (Numeric (Num rn))) -> Right $ PrimType $ Numeric $ Num $ ln ** rn
--    (PrimType(Numeric (MtInt ln)), PrimType (Numeric (Num rn))) -> Right $ makeMtType $ (integerEncodeDouble ln 1) ** rn
--    (PrimType (Numeric (Num ln)), PrimType(Numeric (MtInt rn))) -> Right $ makeMtType $ ln ** (integerEncodeDouble rn 1)
--    (t1, t2) -> Left $ TypeError "^" [t1, t2]
applyBinOp (Comp Eql) l r = Right $ PrimType $ Boolean (l == r)
applyBinOp (Comp NEq) l r = Right $ PrimType $ Boolean (l /= r)
--applyBinOp (Comp GEq) l r = Right $ PrimType $ Boolean (l <= r)
--applyBinOp (Comp GEq) l r = Right $ PrimType $ (Boolean (l <= l))
--applyBinOp (Comp G) l r = Right $ PrimType $ (Boolean (l > r))
--applyBinOp (Comp LEq) l r = Right $ PrimType $ (Boolean (l <= r))
--applyBinOp (Comp L) l r = Right $ PrimType $ (Boolean (l < r))
--applyBinOp op l r = Left $ LitBinOpError op l r

indexMtArr :: MtCon -> MtCon -> Either Error MtType
indexMtArr (MtArray t1 dim elems l1) (MtArray (Numeric (MtInt _)) 1 indexItems indexLen) = do
  case dim >= indexLen of
    True -> case indexItems of
      [ConItem i] -> case i of
        LitE (Numeric (MtInt n)) -> case elems !? (integerToInt n) of
          Just (ConItem (LitE item)) -> Right $ PrimType $ item
          Just (ConInner inner) -> Right $ ConType $ inner
          Nothing -> Left $ IndexError (MtArray t1 dim elems l1) (MtArray (Numeric (MtInt (integerFromInt 1))) 1 indexItems indexLen)
      [] -> Left $ IndexError (MtArray t1 dim elems l1) (MtArray UnkPrim 1 indexItems 0)
    False -> Left $ IndexError (MtArray t1 dim elems l1) (MtArray (Numeric (MtInt (integerFromInt 1))) 1 indexItems indexLen)


applyUniOP :: UniOp -> MtType -> Either Error MtType
applyUniOP Neg n = 
  case n of
    PrimType (Numeric (MtInt num)) -> Right $ makeMtType $ -num
    PrimType (Numeric (Num num)) -> Right $ makeMtType $ -num
    tOther -> Left $ TypeError "-" [n]
applyUniOP Not (PrimType (Boolean b)) = Right $ makeMtType $ not b
applyUniOP Abs n = 
  case n of
    PrimType (Numeric (MtInt num)) -> Right $ makeMtType $ abs num
    PrimType (Numeric (Num num)) -> Right $ makeMtType $ abs num
    tOther -> Left $ TypeError "abs" [n]
--applyUniOP Sin n = 
--  case n of
--    PrimType (Numeric (MtInt num)) -> Right $ makeMtType $ sin num
--    PrimType (Numeric (Num num)) -> Right $ makeMtType $ sin num
--    tOther -> Left $ TypeError "sin" [n]
--applyUniOP Cos n =
--  case n of
--    PrimType (Numeric (MtInt num)) -> Right $ makeMtType $ cos num
--    PrimType (Numeric (Num num)) -> Right $ makeMtType $ cos num
--    tOther -> Left $ TypeError "cos" [n]
--applyUniOP Tan n = 
--  case n of
--    PrimType (Numeric (MtInt num)) -> Right $ makeMtType $ tan num
--    PrimType (Numeric (Num num)) -> Right $ makeMtType $ tan num
--    tOther -> Left $ TypeError "tan" [n]
--applyUniOP Sec n = Right $ 
--  case n of
--    PrimType (Numeric (MtInt num)) -> Right $ makeMtType $ 1 / cos num
--    PrimType (Numeric (Num num)) -> Right $ makeMtType $ 1 / cos num
--    tOther -> Left $ TypeError "sec" [n]
--applyUniOP Csc n = 
--  case n of
--    PrimType (Numeric (MtInt num)) -> Right $ makeMtType $ 1 / sin num
--    PrimType (Numeric (Num num)) -> Right $ makeMtType $ 1 / sin num
--    tother -> Left $ TypeError "csc" [n]
--applyUniOP Ctan n = 
--  case n of
--    PrimType (Numeric (MtInt num)) -> Right $ makeMtType $ 1 / tan num
--    PrimType (Numeric (Num num)) -> Right $ makeMtType $ 1 / tan num
--    tother -> Left $ TypeError "ctan" [n]
applyUniOP op mtType = Left $ TypeError "ctan" [mtType]

evalExpr ::
     Expr
  -> HM.Map String Expr
  -> HM.Map String Function
  -> Int
  -> Either Error MtType
evalExpr (LitE (Numeric (MtInt n))) _ _ _ = Right $ makeMtType n
evalExpr (LitE (Numeric (Num n))) _ _ _ = Right  $ makeMtType n
evalExpr (LitE (Boolean n)) _ _ _ = Right $ makeMtType n
evalExpr (ConE (MtArray arrType dim elems len)) vars fxns gas = do
  evalElem <- mapM (\x -> evalArrayElem x vars fxns gas) elems

  case evalElem of
    ((ConItem (LitE (Boolean _))):rs) -> Right $ ConType $ MtArray (Boolean True) dim evalElem len
    ((ConItem (LitE (Numeric (MtInt _)))):rs) -> Right $ ConType $ MtArray (Numeric (MtInt 1)) dim evalElem len
    ((ConItem (LitE (Numeric (Num _)))):rs) -> Right $ ConType $ MtArray (Numeric (Num 1)) dim evalElem len
    ((ConInner (MtArray conType dim _ _)):rs) -> Right $ ConType $ MtArray conType (dim + 1) evalElem len
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

evalArrayElem :: 
      ConElem 
  ->  HM.Map String Expr
  ->  HM.Map String Function
  ->  Int
  ->  Either Error ConElem
evalArrayElem _ _ _ 0 = Left EmptyExpr -- TODO add error for max recursion depth
evalArrayElem (ConItem expr) vars fxns gas = do
  item <- evalExpr expr vars fxns (gas - 1)
  case item of
    (PrimType t) -> Right $ ConItem $ LitE $ t
    mtType -> Left $ TypeError "arrayItem" [mtType]
evalArrayElem (ConInner con) vars fxns gas = do
  case con of
    (MtArray mtType dim elems len) -> do
      elemItems <- mapM (\x -> evalArrayElem x vars fxns (gas - 1)) elems
      Right $ ConInner $ MtArray mtType dim elemItems len

