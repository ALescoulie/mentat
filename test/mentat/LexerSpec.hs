module LexerSpec where

import Control.Monad
import Mentat.Lexer (lex)
import Mentat.ParseTypes
import Prelude hiding (lex)
import Test.Hspec

spec :: Spec
spec = do
  describe "Lexing single Char Tokens" $ do
    describe "Parses single char BinOps" $ do
      let cases =
            zip
              "+*/^=<>"
              [Add, Mul, Div, Exp, (Comp Eql), (Comp L), (Comp G)]
      forM_ cases $ \(input, expected) ->
        it ("Parses " ++ show input) $ do lex [input] `shouldBe` [TOp expected]
    describe "Parses Brackets" $ do
      let brackets = [Curl, Sqr, Paren]
      let cases = zip "{[(}])" $ map TOpen brackets ++ map TClose brackets
      forM_ cases $ \(input, expected) ->
        it ("Parses " ++ show input) $ do lex [input] `shouldBe` [expected]
  describe "Parse multi Char Tokens" $ do
    let cases =
          [ ("<=", TOp (Comp LEq))
          , (">=", TOp (Comp GEq))
          , ("!=", TOp (Comp NEq))
          , (":=", TAsgn)
          , ("uwu", TId "uwu")
          , ("owo", TId "owo")
          , ("a31b", TId "a31b")
          , ("1312", TNumber 1312.0)
          , ("1.312", TNumber 1.312)
          , ("1.31e2", TNumber 131.0)
          , ("false", TFalse)
          , ("true", TTrue)
          , ("falseowo", TId "falseowo")
          , ("trueuwu", TId "trueuwu")
          , ("sin", TUOp Sin)
          , ("cos", TUOp Cos)
          , ("not", TUOp Not)
          ]
    forM_ cases $ \(input, expected) ->
      it ("Parses " ++ show input) $ do lex input `shouldBe` [expected]
  describe "Parses varriable adjancy multiplication" $ do
    let mulStr = "2x"
    let mulToks = [TNumber 2.0, TOp Mul, TId "x"]
    it ("Parse " ++ show mulStr) $ do lex mulStr `shouldBe` mulToks
  describe "Parses full statments" $ do
    let cases =
          [ ("1 <= 2", [TNumber 1, TOp (Comp LEq), TNumber 2])
          , ( "1 + (1 + 2)"
            , [ TNumber 1
              , TOp Add
              , TOpen Paren
              , TNumber 1
              , TOp Add
              , TNumber 2
              , TClose Paren
              ])
          , ("true = false", [TTrue, TOp (Comp Eql), TFalse])
          , ("y)", [TId "y", TClose Paren])
          ]
    forM_ cases $ \(input, expected) ->
      it ("Parses " ++ show input) $ do lex input `shouldBe` expected
