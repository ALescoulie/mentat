module Mentat.ExpressionParser (parseExpr, resolveNegs) where

import Prelude hiding (lex)

import Control.Monad ((>=>))

import Mentat.ParseTypes
import Mentat.Tokenizer



data NegTree 
  = Empty
  | NegNode NegTree NegTree
  | BinOpNode BinOp NegTree NegTree
  | UniOpNode UniOp NegTree
  | BracketNode Bracket NegTree
  | Leaf TokTree
  deriving (Show, Eq)

-- buildNegTrees [TokTree] -> NegTree -> Either Error [NegTree]
buildNegTree :: [TokTree] -> Either Error NegTree
buildNegTree toks = buildNegTree' toks Empty


buildNegTree' :: [TokTree] -> NegTree -> Either Error NegTree
buildNegTree' [] prev = Right prev
buildNegTree' (TLeaf (TId id): rs) Empty = buildNegTree' rs $ Leaf (TLeaf $ TId id)
buildNegTree' (TLeaf (TNumber n): rs) Empty = buildNegTree' rs $ Leaf (TLeaf $ TNumber n)
buildNegTree' (TFxn name expr: rs) Empty = buildNegTree' rs $ Leaf (TFxn name expr)
buildNegTree' (TNode bracket innerTok: rs) Empty = do
  innerTree <- buildNegTree' innerTok Empty
  buildNegTree' rs $ BracketNode bracket innerTree
buildNegTree' (TLeaf (TUOp op): next : rest) Empty = do
  restT <- buildNegTree' [next] Empty
  buildNegTree' rest $ UniOpNode op restT
buildNegTree' (TLeaf (TOp op): rest) left = do
  case left of
    Empty -> Left UnfinishedTokenStream
    tree -> do
      right <- buildNegTree' rest Empty
      Right $ BinOpNode op left right
buildNegTree' (TLeaf TNeg: rest) left = do
  right <- buildNegTree' rest Empty
  Right $ NegNode left right
  
-- TODO add check for function call arguments
parseNegs :: NegTree -> Either Error NegTree
parseNegs (NegNode left right) = do
  case (left, right) of
    (Empty, Empty) -> Left UnfinishedTokenStream
    (_, Empty) -> Left UnfinishedTokenStream
    (Empty, Leaf token) -> Right $ UniOpNode Neg (Leaf token)
    (_, _) -> do
      l <- parseNegs left
      r <- parseNegs right
      Right $ BinOpNode Sub l r
parseNegs (BinOpNode op left right) = do
  l <- parseNegs left
  r <- parseNegs right
  Right $ BinOpNode op l r
parseNegs (UniOpNode op tree) = do
  child <- parseNegs tree
  Right $ UniOpNode op child
parseNegs (BracketNode bracket tree) = do
  child <- parseNegs tree
  Right $ BracketNode bracket child
parseNegs tree = Right tree


deconstructNegTree :: NegTree -> Either Error [TokTree]
deconstructNegTree Empty = Right []
deconstructNegTree (Leaf tokTree) = Right [tokTree]
deconstructNegTree (BinOpNode op left right) = do
  rightToks <- deconstructNegTree right
  leftToks <- deconstructNegTree left
  Right $ leftToks ++ [(TLeaf (TOp op))] ++ rightToks
deconstructNegTree (NegNode left right) = Left UnfinishedTokenStream -- maybe should not have this
deconstructNegTree (UniOpNode op tree) = do
  child <- deconstructNegTree tree
  Right $ [TLeaf (TUOp op)] ++ child
deconstructNegTree (BracketNode bracket tree) = do
  child <- deconstructNegTree tree
  Right [TNode bracket child]


resolveNegs :: [TokTree] -> Either Error [TokTree]
resolveNegs [] = Right []
resolveNegs tokens = (buildNegTree >=> parseNegs >=> deconstructNegTree) tokens


-- | Parses TokTree list into expression
parseExpr :: [TokTree] -> Either Error Expr
parseExpr tokens = do
  case (TLeaf TNeg) `elem` tokens of
    True -> do
      negResolvedToks <- resolveNegs tokens
      shuntingYard negResolvedToks [] []
    False -> do
      shuntingYard tokens [] []

-- | helper for parseExpr
shuntingYard :: [TokTree] -> [Expr] -> [BinOp] -> Either Error Expr
shuntingYard [] exprs ops
  -- if there are no more tokens
 = do
  (mergedExprs, mergedOps) <- combineExprs exprs ops Nothing -- merge the remaing exprs and ops
  case length mergedExprs == 1 && length mergedOps == 0 -- if there are extra exprs or ops throw error
        of
    True -> Right $ head mergedExprs -- if no extras return expr
    False -> Left EmptyExpr -- if there are extras throw error
shuntingYard (TLeaf (TId id) : TArray items : restT) exprs ops = do 
  container <- parseArr (TArray items)
  let indexExpr = BinOpE Index (VarE id) (ConE container)
  shuntingYard restT (indexExpr : exprs) ops
shuntingYard (toT:restT) exprs ops =
  case toT of
    TLeaf (TNumber n) -> shuntingYard restT (LitE (Numeric (Num n)) : exprs) ops
    TLeaf (TId i) -> shuntingYard restT (VarE i : exprs) ops
    TLeaf (TOp op) -> do
      (mergedExprs, mergedOps) <- combineExprs exprs ops $ Just op
      shuntingYard restT mergedExprs mergedOps
    TNode _ innerTok -> do
      innerExpr <- shuntingYard innerTok [] []
      shuntingYard restT (innerExpr : exprs) ops
    TFxn name args -- if you encounter a fxn
     -> do
      argExprs <- mapM parseExpr args
      shuntingYard restT (FxnE name argExprs : exprs) ops
    TLeaf (TUOp uop) -> do
      case restT of
        (TLeaf (TNumber n): rs) -> do
          let innerExpr = LitE (Numeric (Num n))
          shuntingYard rs (UniOpE uop innerExpr : exprs) ops
        (TLeaf (TId varId) : rs) -> shuntingYard rs (UniOpE uop (VarE varId) : exprs) ops
        (TFxn name args : rs) -> do
          argExprs <- mapM (\x -> shuntingYard x [] []) args
          let fxnCall = FxnE name argExprs
          shuntingYard rs (UniOpE uop fxnCall : exprs) ops
        (TNode _ innerTok : rs) -> do
          innerExpr <- shuntingYard innerTok [] []
          shuntingYard rs (UniOpE uop innerExpr : exprs) ops
        (TArray items : rs) -> do
          container <- parseArr (TArray items)
          shuntingYard rs (UniOpE uop (ConE container) : exprs) ops
        (TLeaf tok : _) -> Left $ BadToken [tok]
    TArray items -> do
      container <- parseArr (TArray items)
      shuntingYard restT (ConE container : exprs) ops

    _ -> Left EmptyExpr

-- | Helper for shuntingYard
buildOpExpr :: BinOp -> Expr -> Expr -> Expr
buildOpExpr op e1 e2 = BinOpE op e1 e2

-- | Helper for shuntingYard
-- | Takes in an expresssion stack operator stack and a maybe BinOp of the last operator hit.
-- | Returns the new Expression Stack and the new Operator Stack.
combineExprs ::
     [Expr] -> [BinOp] -> Maybe BinOp -> Either Error ([Expr], [BinOp])
combineExprs [] [] _ = Left EmptyExpr -- if both stacks are empty, and there is an op
combineExprs [] (op:rOps) _ = Left $ BadOp $ op -- If there are not enough expressions on the stack
combineExprs exprs [] op =
  case op -- If there are exprs left and no ops on the stack
        of
    Nothing ->
      case length exprs == 1 -- If op is Nothing, means all tokens are parsed
            of
        True -> Right (exprs, []) -- if tokens are constructed into a single tree return it
        False -> Left $ BadExpr exprs -- if not return error
    Just op -> Right (exprs, [op]) -- if op add to stack and return
combineExprs (exprR:exprL:rest) (op2:rops) maybeOp =
  case maybeOp -- if there are enought expression on the stack
        of
    Just op1 ->
      case popOp op1 op2 of
        True ->
          combineExprs (buildOpExpr op2 exprL exprR : rest) (rops) $ Just op1 -- If op1 has presidence over op2 combine the two expressions and put the new expression on the stack
        False -> Right ((exprR : exprL : rest), (op1 : op2 : rops))
    Nothing -> do
      combineExprs (buildOpExpr op2 exprL exprR : rest) rops Nothing
combineExprs _ _ _ = Left EmptyExpr

parseArr :: TokTree -> Either Error MtCon
parseArr (TArray items) = do
  elems <- mapM (parseExpr >=> parseConElem) items
  case elems of
    (i:rs) -> case foldl (&&) True $ map (\x -> checkArrElem i x) rs of
      True -> case i of
        (ConItem _) -> Right $ MtArray UnkPrim 1 (i:rs) (length elems)
        (ConInner (MtArray _ dim _ _)) -> Right $ MtArray UnkPrim (dim + 1) (elems) (length elems)
        (ConInner (MtSet t dim _ _)) -> Right $ MtArray t (dim + 1) (elems) (length elems)
      False -> Left $ BadDecl [TArray items]
    [] -> Right $ MtArray UnkPrim 1 [] 0
parseArr tok = Left $ BadDecl [tok]


-- | Given an ConElem it ensures the other ConElem matches
checkArrElem :: ConElem -> ConElem -> Bool
checkArrElem (ConItem _) (ConItem _) = True
checkArrElem (ConInner (MtArray _ dim1 _ _)) (ConInner (MtArray _ dim2 _ _)) = dim1 == dim2
checkArrElem _ _ = False


parseConElem :: Expr -> Either Error ConElem
parseConElem (ConE container) = Right $ ConInner container
parseConElem expr = Right $ ConItem expr
