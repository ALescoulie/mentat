module Mentat.ParseTypes where

import GHC.Int
import GHC.Float
import GHC.Num (integerFromInt)

-- | Errors for parsing and evaluation process
data Error
  = GotNegative
  | GotZero
  | MismatchedBracket [Bracket] [Token]
  | UnclosedBracket [Bracket] [Token]
  | UnfinishedTokenStream
  | BadToken [Token]
  | BadExpr [Expr]
  | BadDecl [TokTree]
  | BadOp BinOp
  | EmptyExpr
  | LitBinOpError BinOp MtType MtType
  | ReasignError String
  | DuplicateVars [String]
  | MissingVar String
  | TypeError String [MtType]
  | IndexError MtCon MtCon

instance Show Error where
  show GotNegative = "you dumbass you put in a negative"
  show GotZero = "you dumbass you put in a zero"
  show (MismatchedBracket stack tokens) =
    "You dumbass you mismatched a bracket.\nstack: " ++
    show stack ++ "\ntokens: " ++ show tokens
  show (UnclosedBracket stack tokens) =
    "You dumbass you forgot a closing bracket.\nstack: " ++
    show stack ++ "\ntokens: " ++ show tokens
  show (BadToken tokens) =
    "You dumbass you put in a bad token.\ntokens: " ++ show tokens
  show (BadExpr expr) =
    "You dumbass you put a bad expression.\nexpr" ++ show expr
  show (BadOp op) = "You dumbass you put a bad operator.\noperator: " ++ show op
  show (LitBinOpError op lExp rExp) =
    "You dumbass you mixed up reals and bools in: \nlExp " ++
    show lExp ++ "\nop" ++ show op ++ " \nrExp" ++ show rExp
  show (ReasignError var) =
    "You dumbass you reasigned a variable.\nvar" ++ show var
  show (BadDecl stream) = "failed to parse decl " ++ show stream
  show (MissingVar var) =
    "You dumbass you forgot to assign variable.\nvar" ++ show var
  show EmptyExpr = "You dumbass you didn't fill out an expression"
  show (TypeError name types) = "You dumbass you cant use the " ++ show name ++ " with these types: " ++ show types
  show (DuplicateVars vars) =
    "You dumbass you repeated a variable: " ++ show vars
  show (IndexError con ind) = "You dumbass you cannot index " ++ show con ++ " with " ++ show ind

  show _ = "You dumbass you caused an error"

-- | Brackets avaliable in code
data Bracket
  = Curl
  | Sqr
  | Paren
  deriving (Show, Eq)

-- | Binary Operators implimented in mentat
data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Exp
  | And
  | Or
  | Xor
  | Comp CompOp
  | Index
  deriving (Show, Eq)


data UniOp
  = Neg
  | Not
  | Abs
  | Sin
  | Cos
  | Tan
  | Sec
  | Csc
  | Ctan
  deriving (Show, Eq)


data CompOp
  = Eql
  | GEq
  | LEq
  | NEq
  | L
  | G
  deriving (Show, Eq)


isCompOp :: BinOp -> Bool
isCompOp (Comp _) = True
isCompOp _ = False


-- | Gives the presidence of BinOp types
opPresidence :: BinOp -> Int
opPresidence Add = 1
opPresidence Sub = 1
opPresidence Mul = 2
opPresidence Div = 2
opPresidence Exp = 3
opPresidence Index = 10000
opPresidence (Comp _) = 0
opPresidence _ = 0

-- | Gives left association of BioOp types
opLeftAssoc :: BinOp -> Bool
opLeftAssoc op
  | op == Exp = False
  | otherwise = True

-- | Takes an operator op1 and the operator from the top of the stack op2
popOp :: BinOp -> BinOp -> Bool
popOp op1 op2
  | op2p > op1p = True
  | op1p == op2p && opLeftAssoc op1 = True
  | otherwise = False
  where
    op1p = opPresidence op1
    op2p = opPresidence op2

-- | The different types of items in a line of mentat code
data Token
  = TNumber Double
  | TOp BinOp
  | TUOp UniOp
  | TOpen Bracket
  | TClose Bracket
  | TSep
  | TSemi
  | TFalse
  | TTrue
  | TId String
  | TAsgn
  | TNeg
  deriving (Show, Eq)


data MtType
  = PrimType MtPrim
  | ConType MtCon
  | Any MtType
  deriving (Show, Eq)

data MtNumeric
  = MtInt Integer
  | Num Double
  deriving (Show, Eq)


data MtPrim
  = Numeric MtNumeric
  | Boolean Bool
  | None
  | UnkPrim
  deriving (Show, Eq)



data MtCon
  = MtArray MtPrim Int [ConElem] Int
  | MtSet MtPrim Int [ConElem] Int
  deriving (Show, Eq)


class MtConvert a where
  makeMtType :: a -> MtType 

instance (MtConvert Int) where 
  makeMtType i = PrimType $ Numeric $ MtInt $ integerFromInt i

instance (MtConvert Double) where 
  makeMtType d = PrimType $ Numeric $ Num d

instance (MtConvert Bool) where
  makeMtType b = PrimType $ Boolean b

instance (MtConvert Integer) where
  makeMtType i = PrimType $ Numeric $ MtInt i


-- | Used as an intermediate step in the parsing process to validate parens and sort them into sub trees
data TokTree
  = TLeaf Token
  | TNode Bracket [TokTree]
  | TFxn String [[TokTree]]
  | TArray [[TokTree]]
  deriving (Show, Eq)

-- | Literal values of float or boolean

data ConElem
  = ConItem Expr
  | ConInner MtCon 
  deriving (Show, Eq)


-- | Mentat expressions which evaluate to a liberal. Literals and varriables are leaves and BinOps are nodes
data Expr
  = LitE MtPrim
  | ConE MtCon
  | VarE String
  | BinOpE BinOp Expr Expr
  | UniOpE UniOp Expr
  | FxnE String [Expr]
  deriving (Show, Eq)

