module EvaluatorSpec where

import Control.Monad (forM_)
import Data.Foldable (toList)
import qualified Data.Map.Strict as HM
import Mentat.Evaluator
import Mentat.Lexer
import Mentat.ParseTypes
import Mentat.SyntaxParser
import Mentat.Tokenizer (parseTokTree)
import Prelude hiding (lex)
import Test.Hspec

spec :: Spec
spec = do
  describe "Testing Expression Evaluation without varriables" $ do
    let cases =
          [ ("1 + 1", RL 2)
          , ("2 + 2 * 3", RL 8)
          , ("(2 + 2) * 3", RL 12)
          , ("(2 + 3 * 2)^2", RL 64)
          , ("2 + 2 * 2^2", RL 10)
          ]
    forM_ cases $ \(input, expected) ->
      it ("Evaluates: " ++ show input) $ do
        let inputExpr = parseTokTree (lex input) >>= parseExpr
        let evalInput = inputExpr >>= \x -> evalExpr x HM.empty HM.empty 1000
        case evalInput of
          Right result -> result `shouldBe` expected
          Left err -> error $ "unexpected error " ++ show err
  describe "Testing Program Evaluation with varriables" $ do
    let pg1Input =
          [ "a := 3"
          , "b := 2"
          , "a + b"
          , "2a + 3b"
          , "a^2 + b^2 = 13"
          , "2a != 3 + a"
          , "(3a)^b"
          ]
    let pg1ExpectedResults = [RL 5, RL 12, BoolL True, BoolL False, RL 81]
    let pg1 = parseProgram pg1Input
    case pg1 of
      Right pg -> do
        let pgExprs = filterExprs $ getProgramStatments pg
        let pgVars = parseVariables pg
        let cases = zip pgExprs pg1ExpectedResults
        forM_ cases $ \(input, expected) -> do
          it ("Evaluates: " ++ show input) $ do
            let result = pgVars >>= \x -> evalExpr input x HM.empty 1000
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
          ]
    let pg2ExpectedResults = [RL 10, RL 26, RL 24, RL 144]
    let pg2 = parseProgram pg2Input
    case pg2 of
      Right pg -> do
        let pgExprs = filterExprs $ getProgramStatments pg
        let pgVars = parseVariables pg
        let pgFxns = parseFxns pg
        let cases = zip pgExprs pg2ExpectedResults
        forM_ cases $ \(input, expected) -> do
          it ("Evaluates: " ++ show input) $ do
            let result =
                  pgFxns >>= (\y -> pgVars >>= (\x -> evalExpr input x y 1000))
            toList result `shouldBe` [expected]
      Left err -> error $ "unexpected error " ++ show err
