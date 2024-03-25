module Mentat.ProgramTypes where

import Mentat.ParseTypes 
import qualified Data.Map.Strict as HM

data EvalType
  = RealT
  | BoolT
  | Any
  deriving (Show, Eq)

-- | A mentat function
-- | Takes the name, argument names, and function expression
data Function = Function String [String] Expr deriving(Show, Eq)


-- | A mentat constraint
-- | This is a mathmatical equation that can be repersented on a graph
-- | Takes a left expression, a right expression, and a comparison operator
data Constraint = Constraint Expr Expr CompOp deriving(Show, Eq)

filterVarStats :: [Statment] -> [Statment]
filterVarStats [] = []
filterVarStats stats = [x | x@(PgVar _ _) <- stats]

filterFxnStats :: [Statment] -> [Statment]
filterFxnStats [] = []
filterFxnStats stats = [x | x@(PgFxn _ _) <- stats]

filterCstrStats :: [Statment] -> [Statment]
filterCstrStats [] = []
filterCstrStats stats = [x | x@(PgCstr _) <- stats]

filterExprStats :: [Statment] -> [Statment]
filterExprStats [] = []
filterExprStats stats = [x | x@(PgExpr _) <- stats]

-- | Used in the Parsing of Programs
data Statment
  = PgVar String Expr
  | PgFxn String Function
  | PgCstr Constraint
  | PgExpr Expr
  deriving (Show)


-- | Stores the information of a mentat program
-- | Takes the list of domain vars, map of vars, map of functions, list of constraints, and list of expressions
data Program = Program [String] (HM.Map String Expr) (HM.Map String Function) [Constraint] [Expr] deriving(Show)

