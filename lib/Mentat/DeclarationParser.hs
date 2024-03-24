module Mentat.DeclarationParser (parseVar, parseFxn) where

import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.ExpressionParser


-- | Parses declerations from TokTree lists
parseVar :: [TokTree] -> Either Error (String, Expr)
parseVar [] = Left EmptyExpr
parseVar (TLeaf (TId i):TLeaf TAsgn:rest) = do
  expr <- parseExpr rest
  Right $ (i, expr)
parseVar other = Left $ BadDecl other

-- | Parse Function
parseFxn :: [TokTree] -> Either Error (String, Function)
parseFxn [] = Left EmptyExpr
parseFxn ((TFxn name args):TLeaf TAsgn:rest) = do
  fxnArgs <- parseFxnArgs args
  fxnExpr <- parseExpr rest
  Right (name, Function name fxnArgs fxnExpr)
parseFxn tokTrees = Left EmptyExpr

parseFxnArgs :: [[TokTree]] -> Either Error [String]
parseFxnArgs [] = Right []
parseFxnArgs ([TLeaf (TId arg)]:restToks) = do
  restArgs <- parseFxnArgs restToks
  Right $ arg : restArgs
parseFxnArgs ([_]:restArgs) = Left UnfinishedTokenStream -- TODO add function parsing errors

