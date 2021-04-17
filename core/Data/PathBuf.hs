{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Provides an opaque byte blob representing a path to a file or directory on disk.
--
-- This type differs from the standard @System.FilePath@ type in several ways:
--
-- * It handles paths that are not valid Unicode. Windows systems use UTF-16 internally
--   for file paths, but do not enforce that surrogate code points must be paired.
--   As such, this type's internal representation is a blob of bytes that is parsed and
--   operated upon without being converted into 'Text' or 'String' values, and it enforces
--   checks for this property at type boundaries.
-- * It avoids the conversion overhead associated with marshalling to the FFI.
--
-- This module is a fairly faithful port of Rust's @std::path::PathBuf@ module. Users who
-- want a higher-level API with fancier types can use 'Data.Path', but this module is
-- intended to be a superior replacement for @System.FilePath@ even if fancy types aren't
-- your thing.
module Data.PathBuf
  ( PathBuf
  -- * Constructors
  , fromBytes
  , fromString
  , fromText
  -- * Conversions
  , toBytes
  , toString
  , toText
  , toText'
  , toTextUnsafe
  -- * Queries
  , isPrefixOf
  , isSuffixOf
  , hasRoot
  , isAbsolute
  , isRelative
  -- * Concatenation
  , join
  -- * Component manipulation
  , lastComponent
  , setLastComponent
  , pathStem
  , setPathStem
  , extension
  , setExtension
  , ancestors
  , components
  , rawComponents
  -- * I/O operations
  , canonicalize
  , exists
  , pointsToDirectory
  , pointsToFile
  , readDirectoryContents
  -- * FFI operations
  , fromCString
  , withCString
  )
  where

import Data.Path.Internal.PathBuf
