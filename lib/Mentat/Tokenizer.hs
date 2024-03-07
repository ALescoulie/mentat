module Mentat.Tokenizer where

import Data.List.Split (splitOn)
import Mentat.ParseTypes

tokSubTree :: TokTree -> [TokTree]
tokSubTree (TNode _ ts) = ts

parseTokTree :: [Token] -> Either Error [TokTree]
parseTokTree tokens =
  case parseInner tokens [] of
    Right ([], t) -> Right t
    Right (_, _) -> Left UnfinishedTokenStream
    Left err -> Left err

-- | Takes in a token stream, and a stack of brackets encountered so far.
-- | Searches from start of token stream up until the next closing bracket.
-- | Returns a tuple of the remaining tokens after the closing bracket, and the TokTree stream before.
parseInner :: [Token] -> [Bracket] -> Either Error ([Token], [TokTree])
parseInner [] [] = Right ([], [])
parseInner [] stack = Left $ UnclosedBracket stack []
parseInner (TClose x:xs) (y:ys)
  | x /= y = Left $ MismatchedBracket (y : ys) (TClose x : xs)
  | otherwise = Right (xs, [])
parseInner (TClose x:xs) [] = Left $ UnclosedBracket [] (TClose x : xs)
parseInner (TId name:TOpen Paren:cRest) stack = do
  let maybeFxn = parseFxnCall (TId name : TOpen Paren : cRest)
  case maybeFxn of
    Right (restAfterFxn, fxnCall) -> do
      (restToks, rest) <- parseInner restAfterFxn stack
      pure (restToks, fxnCall : rest)
    Left _ -> do
      (restTokens, rest) <- parseInner (TOpen Paren : cRest) stack
      pure (restTokens, TLeaf (TId name) : rest)
parseInner (TOpen x:xs) stack
  -- don't need cases since errors are infective
 = do
  (tokAfterInner, inner) <- parseInner xs (x : stack) -- parse inner for bracket x
  (tokAfterRest, rest) <- parseInner tokAfterInner stack -- parse tokens after x
  pure (tokAfterRest, TNode x inner : rest) -- put tuple of results into Either
parseInner (x:xs) stack = do
  (restTokens, rest) <- parseInner xs stack
  pure (restTokens, TLeaf x : rest)

parseFxnCall :: [Token] -> Either Error ([Token], TokTree)
parseFxnCall (TId name:TOpen Paren:TClose Paren:rest) =
  Right (rest, TFxn name [[]])
parseFxnCall (TId name:TOpen Paren:call) = do
  (rest, argToks) <- parseInner call [Paren]
  let args = splitOn [TLeaf TSep] argToks
  Right (rest, TFxn name args)
parseFxnCall _ = Left UnfinishedTokenStream -- add an error
