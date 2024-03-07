module Mentat.ParseTypes where

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
  | LitBinOpError BinOp Literal Literal
  | ReasignError String
  | DuplicateVars [String]
  | MissingVar String

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
  show (DuplicateVars vars) =
    "You dumbass you repeated a variable: " ++ show vars
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
  | Eql
  | GEq
  | LEq
  | NEq
  | L
  | G
  deriving (Show, Eq)

-- | Gives the presidence of BinOp types
opPresidence :: BinOp -> Int
opPresidence Add = 1
opPresidence Sub = 1
opPresidence Mul = 2
opPresidence Div = 2
opPresidence Exp = 3
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
  = TNumber Float
  | TOp BinOp
  | TOpen Bracket
  | TClose Bracket
  | TSep
  | TFalse
  | TTrue
  | TId String
  | TAsgn
  deriving (Show, Eq)

data Function =
  Function String [String] Expr
  deriving (Show, Eq)

-- | Used as an intermediate step in the parsing process to validate parens and sort them into sub trees
data TokTree
  = TLeaf Token
  | TNode Bracket [TokTree]
  | TFxn String [[TokTree]]
  deriving (Show, Eq)

-- | Literal values of float or boolean
data Literal
  = BoolL Bool
  | RL Float
  deriving (Show, Eq)

-- | Mentat expressions which evaluate to a liberal. Literals and varriables are leaves and BinOps are nodes
data Expr
  = LitE Literal
  | VarE String
  | BinOpE BinOp Expr Expr
  | FxnE String [Expr]
  deriving (Show, Eq)

-- | A statement repersents a single line of mentat code which is either a constraint or a decleration
-- | A constraint is an expression that evaluates to a boolean
-- | An assingment assocates a varriable to an ID
data Statment
  = Declaration String Expr
  | Constraint Expr
  | Fxn Function
  deriving (Show, Eq)

-- | A program is a list of statments
newtype Program =
  Program [Statment]
  deriving (Show, Eq)
