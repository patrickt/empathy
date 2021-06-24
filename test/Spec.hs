{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import Gen qualified
import Data.Path.Internal.Component qualified as Component
import Text.Megaparsec qualified as Parsec
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.ByteString.Builder qualified as Builder
import Hedgehog
import qualified Hedgehog.Gen as HG
import System.Exit
import Control.Monad

prop_pathPrefixesRoundtrip :: Property
prop_pathPrefixesRoundtrip = property do
  prefix <- forAll Gen.prefix
  -- NOTE(chb): Not sure if this should go here, but I needed to test it sorry
  trailingSlash <- forAll $ HG.maybe (return '\\') -- TODO: trailing forward slash (if non-verbatim)
  let prefixToBS = BLC.toStrict . Builder.toLazyByteString . maybe id (\s b -> b <> Builder.char8 s) trailingSlash .  Component.renderPrefix
  tripping prefix prefixToBS (Parsec.parse Component.parsePrefix "'")

main :: IO ()
main = do
  ok <- checkParallel $$(discover)
  unless ok exitFailure
