{-# LANGUAGE AllowAmbiguousTypes, DataKinds, TypeApplications, ScopedTypeVariables #-}

module Main (main) where

import qualified Test.Tasty as Tasty
import           Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as HUnit
import           Data.Path (Path, (</>), Anchor (..), Entity (..))
import qualified Data.Path as Path

assertParsesInto :: (Path.AbsRel ar, Path.FileDir fd) => String -> Path ar fd -> HUnit.Assertion
assertParsesInto a b = case Path.parse a of
  Left s  -> HUnit.assertFailure ("error message: " <> s)
  Right p -> p @?= b

assertFailsParseInto :: forall ar fd . (Path.AbsRel ar, Path.FileDir fd) => String -> HUnit.Assertion
assertFailsParseInto a = case Path.parse @ar @fd a of
  Left _  -> pure ()
  Right p -> HUnit.assertFailure ("expected failure in parsing " <> Path.toString p)

test_introduce :: Tasty.TestTree
test_introduce = Tasty.testGroup "introduction"
  [ HUnit.testCase "parse '/' == rootDir" ("/" `assertParsesInto` Path.rootDir)
  , HUnit.testCase "parse '/tmp'" ("/tmp" `assertParsesInto` (Path.rootDir </> Path.relDir @"tmp"))
  , HUnit.testCase "parse '/tmp/" ("/tmp/" `assertParsesInto` (Path.rootDir </> Path.relDir @"tmp"))
  , HUnit.testCase "fails parsing /tmp as relative directory" (assertFailsParseInto @'Rel @'Dir "/tmp")
  , HUnit.testCase "fails parsing /tmp/ as relative file" (assertFailsParseInto @'Rel @'File "/tmp/")
  , HUnit.testCase "fails parsing /tmp/ as absolute file" (assertFailsParseInto @'Abs @'File "/tmp/")
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
