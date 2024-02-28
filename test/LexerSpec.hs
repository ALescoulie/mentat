module LexerSpec where

import Control.Monad
import Mentat.Lexer (lex)
import Mentat.ParseTypes
import Prelude hiding (lex)
import Test.Hspec

spec :: Spec
spec = do
  describe "Lexing BinOps" $ do
    describe "Parses single char BinOps" $ do
      let cases = zip "+-*/^=<>" [Add, Sub, Mul, Div, Exp, Eql, L, G]
      forM_ cases $ \(input, expected) ->
        it ("Parses " ++ show input) $ do lex [input] `shouldBe` [TOp expected]
    describe "Parse multi char Tokens" $ do
      let cases =
            [ ("<=", TOp LEq)
            , (">=", TOp GEq)
            , ("!=", TOp NEq)
            , (":=", TAsgn)
            , ("uwu", TId "uwu")
            , ("owo", TId "owo")
            , ("a31b", TId "a31b")
            , ("1312", TNumber 1312.0)
            , ("1.312", TNumber 1.312)
            , ("1.31e2", TNumber 131.0)
            ]
      forM_ cases $ \(input, expected) ->
        it ("Parses " ++ show input) $ do lex input `shouldBe` [expected]
