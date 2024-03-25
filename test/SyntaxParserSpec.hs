module SyntaxParserSpec where

import qualified Data.Map.Strict as HM
import Mentat.ParseTypes
import Mentat.ParseTypes (Statment(Constraint))
import Mentat.SyntaxParser
import Prelude hiding (lex)
import Test.Hspec

spec :: Spec
spec = do
  describe "Testing whole program parsing" $ do
    let pg1 = ["x := 1", "y := 2", "f(n) := 2 * n", "2 = x * y", "f(2x)"]
    let out1 =
          Program
            [ Declaration "x" (LitE $ RL 1)
            , Declaration "y" (LitE $ RL 2)
            , Fxn $ Function "f" ["n"] (BinOpE Mul (VarE "n") (LitE $ RL 2))
            , Constraint
                (BinOpE Eql (BinOpE Mul (VarE "y") (VarE "x")) (LitE $ RL 2))
            , Constraint $ FxnE "f" [(BinOpE Mul (VarE "x") (LitE $ RL 2))]
            ]
    let maybePg1 = parseProgram pg1
    it ("Parses program: " ++ show pg1) $ do
      case maybePg1 of
        Right pg -> pg `shouldBe` out1
        Left err -> error $ "Unexepected error " ++ show err
    it "Parses varriables x and y: " $ do
      case maybePg1 of
        Right pg ->
          case parseVariables pg of
            Right vars ->
              case (HM.lookup "x" vars, HM.lookup "y" vars) of
                (Nothing, _) -> error "error: x not found in vars lookup"
                (_, Nothing) -> error "error: y not found in vars loopup"
                (Just x, Just y) -> (x, y) `shouldBe` (LitE $ RL 1, LitE $ RL 2)
        Left err -> error $ "Unexepected error " ++ show err
