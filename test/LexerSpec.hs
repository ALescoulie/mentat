module LexerSpec where

import Prelude hiding ( lex )
import Test.Hspec
import Mentat.Lexer ( lex )
import Mentat.ParseTypes 
import Control.Monad

spec :: Spec
spec = do
  describe "Lexing BinOps" $ do
    describe "Parses single char BinOps" $ do
      let cases = zip (map (\x -> [x]) "+-*/^=<>") $ map (\x -> [TOp x]) [ Add, Sub, Mul, Div, Exp, Eql, L, G]
      forM_ cases $ \(input, expected) ->
        it ("parses " ++ show input) $ do
          lex input `shouldBe` expected



