module Mentat.ConstraintParser (parseConstraint) where

import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.ExpressionParser


containsCompOp :: Expr -> Bool
containsCompOp (BinOpE (Comp _) left right) = True
containsCompOp (BinOpE op left right) = containsCompOp left || containsCompOp right
containsCompOp _ = False


parseConstraint :: [TokTree] -> Either Error Constraint
parseConstraint [] = Left EmptyExpr
parseConstraint tokTree = do
  expr <- parseExpr tokTree

  case expr of
    BinOpE op left right -> do
      case op of
        Comp comp -> do
            if containsCompOp left || containsCompOp right then
              Left EmptyExpr else Right $ Constraint left right comp
        _ -> Left EmptyExpr
    _ -> Left EmptyExpr
