{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Mentat.FunctionBuilder where

import GHC.Generics
import Mentat.ParseTypes
import Mentat.ProgramTypes
import Mentat.Program
import Mentat.Evaluator
import Mentat.Validator
import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as HM
import Data.Aeson

-- | by conventaion all names will start with "mentatFunc" then have the function name
-- | the hash map containing varriable values will always be the first argument of a fucntion and be called "mentatVars"

-- | The inLT.unpack $ formation needed to construct a javascript function 
-- | Takes the name,  n_args, arg_names, body


translateLit :: Literal -> String
translateLit (RL n) = show n
translateLit (BoolL True) = "true"
translateLit (BoolL False) = "false"


translateVar :: String -> String
translateVar s = "mentatVars.get(" ++ s ++ ")"


translateOp :: BinOp -> String
translateOp Add = "+"
translateOp Sub = "-"
translateOp Mul = "*"
translateOp Div = "/"
translateOp Exp = "**"
translateOp (Comp Eql) = "==="
translateOp (Comp NEq) = "!="
translateOp (Comp GEq) = ">="
translateOp (Comp G) = ">"
translateOp (Comp LEq) = "<="
translateOp (Comp L) = "<"


translateArgString :: [String] -> String
translateArgString [] = ""
translateArgString (s: ss) = if null ss then s else s ++ ", " ++ translateArgString ss

translateExpr :: Expr -> [String] -> String
translateExpr (LitE lit) _ = translateLit lit
translateExpr (VarE var) args = if var `elem` args then var else translateVar var
translateExpr (BinOpE op (LitE left) (LitE right)) _ =
  case applyBinOp op left right of
    Right result -> translateLit result
    Left err -> error $ "unexpected error: " ++ show err
translateExpr (BinOpE op left right) args = do
  let leftText = translateExpr left args
  let rightText = translateExpr right args
  let opStr = translateOp op
  "(" ++ leftText ++ ")" ++ opStr ++ "(" ++ rightText ++ ")"
translateExpr (FxnE name argExprs) args = do
  let transArgs = map (\x -> translateExpr x []) argExprs
  let argStr = translateArgString $ ["mentatArgs", "mentatFuncs"] ++ transArgs
  " mentatFunc.get(" ++ name ++  ")(" ++ argStr ++ ")"


data TransFunction = 
  TransFunction { name :: String
                , args :: [String]
                , body :: String
                  } deriving(Show, Eq, Generic)

instance ToJSON TransFunction

translateFunction :: Function -> TransFunction
translateFunction (Function name args expr) = do
  let transExpr = translateExpr expr args
  let transArgs = ["mentatVars", "mentatFuncs"] ++ args
  let body = "return (" ++ transExpr ++ ")"
  TransFunction name transArgs transExpr


data TransConstraint =
  TransConstraint { left :: TransFunction
                  , right :: TransFunction
                  , comparison :: String
                    } deriving(Show, Eq, Generic)

instance ToJSON TransConstraint

translateConstraint :: Constraint -> [String] -> TransConstraint
translateConstraint (Constraint left right comp) domainVars = do
  let leftFxn = TransFunction "MentatExprLeft" (["mentatVars", "MentatFuncs"] ++ domainVars) (translateExpr left domainVars) 
  let rightFxn = TransFunction "MentatExprLeft" (["mentatVars", "MentatFuncs"] ++ domainVars) (translateExpr right domainVars)
  TransConstraint leftFxn rightFxn $ show comp

-- TODO: figure out which translit to use for json conversion later on
data TransLit 
  = TFloat Float
  | TBool Bool deriving (Show, Generic)

instance ToJSON TransLit

translateLiteral :: Literal -> TransLit
translateLiteral (RL n) = TFloat n
translateLiteral (BoolL b) = TBool b


translateVars :: Program -> Either Error [(String, TransLit)]
translateVars pg = do
  let vars = getPgVars pg
  let fxns = getPgFxns pg

  let varsList = HM.toList vars
  let varMaybeEvaled = map (\(n, x) -> (n, evalExpr x vars fxns 1000)) varsList
  let varNames = map (\(n, _) -> n) varMaybeEvaled
  varVals <- sequence $ map (\(_, x) -> fmap translateLiteral x) varMaybeEvaled
  Right $ zip varNames varVals


data TransProgram =
  TransProgram { varriables :: [(String, TransLit)]
               , functions :: [TransFunction]
               , constraints :: [TransConstraint]
               , expressions :: [String]
                 } deriving(Show, Generic)

instance ToJSON TransProgram

-- | Takes in a program and outputs translated functions, constraints and expressions
translateProgram :: Program -> [String] -> Either Error TransProgram
translateProgram pg domainVars = do
  
  transVars <- translateVars pg

  let funcs = getPgFxns pg
  let transFuncs = map translateFunction $ HM.elems funcs

  let cstrs = getPgCstrs pg
  
  let transCstrs = map (\x -> translateConstraint x domainVars) cstrs
  
  let transExpr = map (\x -> translateExpr x []) $ getPgExprs pg

  Right $ TransProgram transVars transFuncs transCstrs transExpr

