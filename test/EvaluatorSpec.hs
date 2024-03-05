module EvaluatorSpec where

import Prelude hiding ( lex )
import Mentat.Lexer
import Mentat.ParseTypes
import Mentat.SyntaxParser
import Mentat.Evaluator
import qualified Data.Map.Strict as HM
import Control.Monad ( forM_ )
import Test.Hspec
import Mentat.Tokenizer ( parseTokTree )

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
        let evalInput = inputExpr >>= \x -> evalExpr x HM.empty
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
            let result = pgVars >>= \x -> evalExpr input x
            expected `elem` result `shouldBe` True
      Left err -> error $ "unexpected error " ++ show err
