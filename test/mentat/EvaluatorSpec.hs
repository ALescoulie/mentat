module EvaluatorSpec where

import Control.Monad (forM_)
import Data.Foldable (toList)
import qualified Data.Map.Strict as HM
import Mentat.Evaluator
import Mentat.Lexer
import Mentat.ParseTypes
import Mentat.Program
import Mentat.ProgramTypes
import Mentat.Tokenizer (parseTokTree)
import Prelude hiding (lex)
import Test.Hspec

spec :: Spec
spec = do
  describe "Testing Program Evaluation with varriables" $ do
    let pg1Input =
          [ "a := 3"
          , "b := 2"
          , "a + b"
          , "2a + 3b"
          , "a^2 + b^2"
          , "3 + a"
          , "(3a)^b"
          ]
    let pg1ExpectedResults = [RL 5, RL 12, RL 13, RL 6, RL 81]
    let pg1 = parseProgram pg1Input []
    case pg1 of
      Right pg -> do
        let pgExprs = getPgExprs pg
        let pgVars = getPgVars pg
        let cases = zip pgExprs pg1ExpectedResults
        forM_ cases $ \(input, expected) -> do
          it ("Evaluates: " ++ show input) $ do
            let result = evalExpr input pgVars HM.empty 1000
            toList result `shouldBe` [expected]
      Left err -> error $ "unexpected error " ++ show err
  describe "Testing Expression Evaluation with functions and varriables" $ do
    let pg2Input =
          [ "a := 3"
          , "b := 2"
          , "f(x) := 2x"
          , "g(x) := 2a * x"
          , "h(x, y) := x^y"
          , "f(a) + f(b)"
          , "f(h(a,2) + h(b,2))"
          , "f(g(b))"
          , "f(g(b) * f(a))"
          , "1 - - 1"
          , "2 - 1"
          , "-5"
          ]
    let pg2ExpectedResults = [RL 10, RL 26, RL 24, RL 144, RL 2, RL 1, RL (-5)]
    let pg2 = parseProgram pg2Input []
    case pg2 of
      Right pg -> do
        let pgExprs = getPgExprs pg
        let pgVars = getPgVars pg
        let pgFxns = getPgFxns pg
        let cases = zip pgExprs pg2ExpectedResults
        forM_ cases $ \(input, expected) -> do
          it ("Evaluates: " ++ show input) $ do
            let result = evalExpr input pgVars pgFxns 1000
            toList result `shouldBe` [expected]
      Left err -> error $ "unexpected error " ++ show err
