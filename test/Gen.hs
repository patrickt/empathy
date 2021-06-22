{-# LANGUAGE ImportQualifiedPost #-}

module Gen (module Gen) where

import Data.Char (ord)
import Data.Path.Internal.Component
import Data.Path.Internal.Raw
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

prefix :: Gen Prefix
prefix =
  let path = asciiRawFilePath
      letter = fromIntegral . ord <$> Gen.alpha
   in Gen.choice
        [ Verbatim <$> path,
          VerbatimUNC <$> path <*> path,
          VerbatimDisk <$> letter,
          DeviceNS <$> path,
          UNC <$> path <*> path,
          Disk <$> letter
        ]

asciiRawFilePath :: Gen RawFilePath
asciiRawFilePath = Gen.utf8 (Range.linear 1 25) Gen.alphaNum
