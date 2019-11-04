module Main (main) where

import qualified Test.Tasty as Tasty
import           Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as HUnit
import           Data.Path (Path)
import qualified Data.Path as Path

assertParsesInto :: (Path.AbsRel ar, Path.FileDir fd) => String -> Path ar fd -> HUnit.Assertion
assertParsesInto a b = case Path.parse a of
  Left s  -> HUnit.assertFailure ("error message: " <> s)
  Right p -> p @?= b

test_introduce :: Tasty.TestTree
test_introduce = Tasty.testGroup "introduction"
  [ HUnit.testCase "parse / == rootDir" ("/" `assertParsesInto` Path.rootDir)
  ]

test_eliminate :: Tasty.TestTree
test_eliminate = Tasty.testGroup "elimination"
  [ HUnit.testCase "root == /" (Path.toString Path.rootDir @?= "/")
  ]


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "empathy"
  [ test_introduce
  , test_eliminate
  ]
