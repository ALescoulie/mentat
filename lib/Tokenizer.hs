module Tokenizer where
import ParseTypes

tokSubTree :: TokTree -> [TokTree]
tokSubTree (TNode _ ts) = ts

parseTokTree :: [Token] -> Either Error [TokTree]
parseTokTree tokens = case parseInner tokens [] of
  Right ([], t) -> Right t
  Right (left, _) -> Left UnfinishedTokenStream
  Left err -> Left err

-- | Takes in a token stream, and a stack of brackets encountered so far.
-- | Searches from start of token stream up until the next closing bracket.
-- | Returns a tuple of the remaining tokens after the closing bracket, and the TokTree stream before.
parseInner :: [Token] -> [Bracket] -> Either Error ([Token], [TokTree])
parseInner [] [] = Right ([], [])
parseInner [] stack = Left $ UnclosedBracket stack []
parseInner (TClose x : xs) (y : ys)
  | x /= y = Left $ MismatchedBracket (y : ys) (TClose x : xs)
  | otherwise = Right (xs, [])
parseInner (TClose x : xs) [] = Left $ UnclosedBracket [] (TClose x : xs)
parseInner (TOpen x : xs) stack = do
  -- don't need cases since errors are infective
  (tokAfterInner, inner) <- parseInner xs (x : stack) -- parse inner for bracket x
  (tokAfterRest, rest) <- parseInner tokAfterInner stack -- parse tokens after x
  pure (tokAfterRest, TNode x inner : rest) -- put tuple of results into Either
parseInner (x : xs) stack = do
  (restTokens, rest) <- parseInner xs stack
  pure (restTokens, TLeaf x : rest)

