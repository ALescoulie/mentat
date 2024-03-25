module SyntaxParserSpec where

import qualified Data.Map.Strict as HM
import Mentat.ParseTypes
import Mentat.Program
import Mentat.ProgramTypes
import Prelude hiding (lex)
import Test.Hspec

spec :: Spec
spec = do
  describe "Testing whole program parsing" $ do
    let pg1 = ["x := sin 1", "y := 2", "f(n) := 2 * n", "2 = x * y", "f(2x)"]
    let pg1Func = Function "f" ["n"] (BinOpE Mul (LitE $ RL 2) (VarE "n"))
    let pg1Cstr =
          Constraint (BinOpE Mul (VarE "y") (VarE "x")) (LitE $ RL 2) Eql
    let pg1Expr = FxnE "f" [(BinOpE Mul (VarE "x") (LitE $ RL 2))]
    let maybePg1 = parseProgram pg1 []
    it ("Parses function: " ++ show pg1Func) $ do
      case maybePg1 of
        Right pg -> do
          let pgFxns = getPgFxns pg
          case (HM.lookup "f" pgFxns) of
            Just f -> f `shouldBe` pg1Func
            Nothing -> error "function f not found in lookup"
        Left err -> error $ "Unexepected error " ++ show err
    it "Parses varriables x and y: " $ do
      case maybePg1 of
        Right pg -> do
          let vars = getPgVars pg
          case (HM.lookup "x" vars, HM.lookup "y" vars) of
            (Nothing, _) -> error "error: x not found in vars lookup"
            (_, Nothing) -> error "error: y not found in vars loopup"
            (Just x, Just y) -> (x, y) `shouldBe` (UniOpE Sin $ LitE $ RL 1, LitE $ RL 2)
        Left err -> error $ "Unexepected error " ++ show err
