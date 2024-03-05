module TokenizerSpec where

import Control.Monad
import Mentat.Lexer
import Mentat.ParseTypes
import Mentat.Tokenizer
import Prelude hiding (lex)
import Test.Hspec
import Data.Foldable (concat)

concatTokenList :: [Token] -> String
concatTokenList [] = ""
concatTokenList (x : xs) = (' ' : show x) ++ concatTokenList xs

spec :: Spec
spec = do
  describe "Parsing simple Token List into TokTrees" $ do
    let inputs =
          [ [TId "x", TOp Add, TId "y"]
          , [TTrue, TOp Eql, TTrue]
          , [TId "x", TAsgn, TNumber 5.0]
          ] 
    let results = map (\x -> map (\y -> TLeaf y) x) inputs

    let cases = zip inputs results
    forM_ cases $ \(input, expected) ->
      it ("Parses" ++ concatTokenList input) $ do
        let tokTreeResult = parseTokTree input
        case tokTreeResult of
          Right tokTrees -> tokTrees `shouldBe` expected
          Left err -> error $ "unexpected error " ++ show err
  describe "Parsing Token list with with parens into sub trees" $ do
    let inputs = 
          map lex
            [ "o + (w + o)"
            , "n + {y + [a + (a)]}"
            ] 

    let results = 
          [ [ TLeaf $ TId "o"
            , TLeaf $ TOp Add
            , TNode Paren
              [ TLeaf $ TId "w"
              , TLeaf $ TOp Add
              , TLeaf $ TId "o"
              ]
            ],
            [ TLeaf $ TId "n"
            , TLeaf $ TOp Add
            , TNode Curl
              [ TLeaf $ TId "y"
              , TLeaf $ TOp Add
              , TNode Sqr
                [ TLeaf $ TId "a"
                , TLeaf $ TOp Add
                , TNode Paren [TLeaf $ TId "a"]
                ]
              ]
            ]
          ]

    let cases = zip inputs results
    forM_ cases $ \(input, expected) -> 
      it ("Parses" ++ concatTokenList input) $ do
        let tokTreeResult = parseTokTree input
        case tokTreeResult of
          Right tokTrees -> tokTrees `shouldBe` expected
          Left err -> error $ "unexpected error " ++ show err

