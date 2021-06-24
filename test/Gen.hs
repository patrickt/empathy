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
      drive = fromIntegral . ord <$> Gen.upper
   in Gen.choice
        [ Verbatim <$> path,
          VerbatimUNC <$> path <*> path,
          VerbatimDisk <$> drive,
          DeviceNS <$> path,
          UNC <$> path <*> path,
          Disk <$> drive
        ]

asciiRawFilePath :: Gen RawFilePath
asciiRawFilePath = Gen.utf8 (Range.linear 1 25) Gen.alphaNum
