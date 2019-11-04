module Main (main) where

import qualified Test.Tasty as Tasty
import           Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.HUnit as HUnit
import qualified Data.Path as Path

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "empathy"
  [ HUnit.testCase "root == /" (Path.toString Path.rootDir @=? "/")
  ]
