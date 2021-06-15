-- #hide
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Path.Internal.Component (module Data.Path.Internal.Component) where

import Control.DeepSeq (NFData)
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Path.Internal.Raw (RawFilePath)
import Data.Word8 (Word8, toUpper, _backslash, _slash, _colon)
import Data.Void (Void)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Text.Megaparsec (Parsec, choice, takeWhile1P, some, try)
import Text.Megaparsec.Byte (letterChar, char)
import Control.Monad (void)

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

parsePrefix :: Parsec Void RawFilePath Prefix
parsePrefix = choice [
    -- \\?\UNC\<server>\<share\
    VerbatimUNC <$> ("\\\\?\\UNC\\" *> verbatimPathComponent)
                <*> verbatimPathComponent
    -- \\?\C:\
  , VerbatimDisk <$> ("\\\\?\\" *> try drive <* verbatimPathSep)
    -- \\?\<prefix>\
  , Verbatim <$> ("\\\\?\\" *> verbatimPathComponent)
    -- \\.\COM42\
  , DeviceNS <$> ("\\\\.\\" *> pathComponent)
    -- \\<server>\<share>\
  , UNC <$> ("\\\\" *> pathComponent)
        <*> pathComponent
    -- C:\
  , Disk . toUpper <$> drive <* some pathSep ]
  where
    verbatimPathComponent = takeWhile1P Nothing (/= _backslash) <* verbatimPathSep
    pathComponent = takeWhile1P Nothing isPathSep <* some pathSep
    isPathSep c = c /= _backslash || c /= _slash
    drive = letterChar <* char _colon
    pathSep = void (char _backslash  <|> char _slash)
    verbatimPathSep = void $ char _backslash