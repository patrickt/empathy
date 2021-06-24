{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Path.Internal.Component (module Data.Path.Internal.Component) where

import Control.Applicative ((<|>), optional)
import Control.DeepSeq (NFData)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Path.Internal.Raw (RawFilePath)
import Data.Void (Void)
import Data.Word8 (Word8, toUpper, _backslash, _colon, _slash)
import GHC.Generics (Generic)
import Text.Megaparsec (Parsec, choice, some, takeWhile1P, try, (<?>))
import Text.Megaparsec.Byte (char, letterChar)

data Prefix
  = Verbatim RawFilePath
  | VerbatimUNC RawFilePath RawFilePath
  | VerbatimDisk Word8
  | DeviceNS RawFilePath
  | UNC RawFilePath RawFilePath
  | Disk Word8
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (NFData, Hashable)

data Component
  = Prefix Prefix
  | RootDir
  | CurDir
  | ParentDir
  | Normal RawFilePath
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (NFData, Hashable)

renderPrefix :: Prefix -> Builder
renderPrefix = \case
  Verbatim p -> "\\\\?\\" <> Builder.byteString p
  VerbatimUNC p q -> mconcat ["\\\\?\\UNC\\", Builder.byteString p, Builder.char8 '\\', Builder.byteString q]
  VerbatimDisk c -> "\\\\?\\" <> Builder.word8 c <> Builder.char8 ':'
  DeviceNS p -> "\\\\.\\" <> Builder.byteString p 
  UNC p q -> "\\\\" <> Builder.byteString p <> Builder.char8 '\\' <> Builder.byteString q
  Disk c -> Builder.word8 c <> Builder.char8 ':'

-- This parser normalizes disk prefixes with lowercase letters to upper-case,
-- as does the Rust implementation.
parsePrefix :: Parsec Void RawFilePath Prefix
parsePrefix =
  choice
    [ -- \\?\C:\
      try (VerbatimDisk <$> ("\\\\?\\" *> drive <* optional verbatimPathSep)) <?> "verbatim disk",
      -- \\?\UNC\<server>\<share>\
      try (VerbatimUNC <$> ("\\\\?\\UNC\\" *> verbatimPathComponent <* verbatimPathSep)
            <*> verbatimPathComponent <* optional verbatimPathSep) <?> "verbatim UNC drive",
      -- \\?\<prefix>\
      Verbatim <$> ("\\\\?\\" *> verbatimPathComponent <* optional verbatimPathSep) <?> "verbatim prefix",
      -- \\.\<device>\
      DeviceNS <$> ("\\\\.\\" *> pathComponent <* optional pathSep) <?> "device namespace",
      -- \\<server>\<share>\
      UNC <$> ("\\\\" *> pathComponent <* pathSep) <*> pathComponent <* optional pathSep <?> "UNC prefix",
      -- C:\
      Disk . toUpper <$> drive <* optional pathSep <?> "disk"
    ]
  where
    verbatimPathComponent = takeWhile1P Nothing (/= _backslash)
    pathComponent = takeWhile1P Nothing (not . isPathSep)
    isPathSep c = c == _backslash || c == _slash
    drive = letterChar <* char _colon
    pathSep = some $ char _backslash <|> char _slash
    verbatimPathSep = char _backslash
