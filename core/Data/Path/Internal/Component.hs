-- #hide
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Path.Internal.Component (module Data.Path.Internal.Component) where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import RawFilePath

data Component
  = Prefix RawFilePath -- TODO: port std::Path::Prefix
  | RootDir
  | CurDir
  | ParentDir
  | Normal RawFilePath
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (NFData, Hashable)
