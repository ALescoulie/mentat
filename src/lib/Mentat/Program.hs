module Mentat.Program (parseProgram, getPgDomainVars, getPgVars, getPgFxns, getPgCstrs, getPgExprs) where

import Prelude hiding (lex)
import Mentat.Lexer (lex)

import Mentat.ParseTypes
import Mentat.ProgramTypes

import Mentat.Tokenizer

import Mentat.ExpressionParser
import Mentat.DeclarationParser
import Mentat.ConstraintParser
import Mentat.Validator
import qualified Data.Map.Strict as HM

-- | parses a list of strings into a program
-- | is a helper for parse program
parseProgram' :: [String] -> Either Error [Statment]
parseProgram' [] = Right []
parseProgram' (s:ss) = do
  let tokens = lex s
  tokTree <- parseTokTree tokens
  let maybeVar = parseVar tokTree
  restStatments <- parseProgram' ss
  case maybeVar of
    Left _ -> do
      let maybeFunction = parseFxn tokTree
      case maybeFunction of
        Left _ -> do
          let maybeConstraint = parseConstraint tokTree
          case maybeConstraint of
            Left _ -> do
              expr <- parseExpr tokTree
              Right $ (PgExpr expr) : restStatments
            Right cstr -> Right $ (PgCstr cstr) : restStatments
        Right (name, fxn) -> Right $ (PgFxn name fxn) : restStatments
    Right (name, val) -> Right $ (PgVar name val) : restStatments

-- | helper for parseProgram
repeatItems :: Eq a => [a] -> [a]
repeatItems [] = []
repeatItems (x:xs) = filter (== x) xs ++ repeatItems xs


-- | Parses and validates a list of strings into a program
parseProgram :: [String] -> [String] -> Either Error Program
parseProgram [] domainVars = Right (Program domainVars HM.empty HM.empty [] [])
parseProgram pg domainVars = do
  programStats <- parseProgram' pg
  let vars = HM.fromList $ map (\(PgVar name val) -> (name, val)) $ filterVarStats programStats
  let fxns = HM.fromList $ map (\(PgFxn name fxn) -> (name, fxn)) $ filterFxnStats programStats
  let exprs = map (\(PgExpr expr) -> expr) $ filterExprStats programStats
  let cstrs = map (\(PgCstr cstr) -> cstr) $ filterCstrStats programStats
  let repeatNames = repeatItems $ (HM.keys vars) ++ (HM.keys fxns)
  if null repeatNames
    then do
      _ <- validateProgram programStats domainVars
      Right $ Program domainVars vars fxns cstrs exprs
    else Left $ DuplicateVars repeatNames


-- | returns the domainVars of the program

getPgDomainVars :: Program -> [String]
getPgDomainVars (Program vars _ _ _ _) = vars


-- | returns the hash map of program varriables
getPgVars :: Program -> HM.Map String Expr
getPgVars (Program _ vars _ _ _) = vars


-- | returns the hash map of program functions
getPgFxns :: Program -> HM.Map String Function
getPgFxns (Program _ _ fxns _ _) = fxns


-- | returns the list of program constraints
getPgCstrs :: Program -> [Constraint]
getPgCstrs (Program _ _ _ cstrs _) = cstrs


-- | returns the list of program expressions
getPgExprs :: Program -> [Expr]
getPgExprs (Program _ _ _ _ exprs) = exprs

