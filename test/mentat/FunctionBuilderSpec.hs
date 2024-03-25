module FunctionBuilderSpec where

import Data.Aeson
import Mentat.FunctionBuilder
import Mentat.ParseTypes
import Mentat.Program
import Mentat.ProgramTypes
import Data.Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8)
import Test.Hspec

spec :: Spec
spec = do
  describe "Test translate Expr" $ do
    let pg1 =
          parseProgram
            [ "a := 1"
            , "b := 2"
            , "f(x) := (2a + 2b)^x"
            , "2x + y"
            , "y = 2x"
            , "f(2) + 1"
            ]
            ["x", "y"]
    let pg1Trans = pg1 >>= \x -> translateProgram x ["x", "y"]
    case pg1Trans of
      Left err -> error $ "unexpected error" ++ show err
      Right (TransProgram vars fxns cstrs exprs) -> do
        it "Parses functions" $ do
          fxns `shouldBe`
            [ (TransFunction
                 "f"
                 ["mentatVars", "mentatFuncs", "x"]
                 "(((2.0)*(mentatVars.get(a)))+((2.0)*(mentatVars.get(b))))**(x)")
            ]
        it "Parses constraints" $ do
          cstrs `shouldBe`
            [ TransConstraint
                (TransFunction
                   "MentatExprLeft"
                   ["mentatVars", "MentatFuncs", "x", "y"]
                   "y")
                (TransFunction
                   "MentatExprLeft"
                   ["mentatVars", "MentatFuncs", "x", "y"]
                   "(2.0)*(x)") 
                "Eql"
            ]
--        it "Translates to JSON" $ do
--          let pgJSON = decodeUtf8 $ encode $ TransProgram vars fxns cstrs exprs
--          let pgExpected = pack "
--                  
--          pgJSON `shouldBe` pgExpected
