{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Provides an opaque byte blob representing a path to a file or directory on disk.
--
-- This type differs from the standard @System.FilePath@ type in several ways, the most
-- prominent of which is that its internal representation is stored as an opaque blob
-- of bytes (a 'RawFilePath', a type alias for 'ByteString'). This is necessary because
-- Windows systems allow filenames to be [ill-formed Unicode](https://simonsapin.github.io/wtf-8/#well-formed).
-- For more information about when and why this is necessary, consult the
-- [WTF-8 encoding standard](https://simonsapin.github.io/wtf-8).
--
-- Even if you don't plan to deploy your code to Windows, this module aims to be an improvement on
-- @System.FilePath@, most notably insofar as it is more efficient, since its 'RawFilePath' internals
-- can be passed to native and FFI interfaces in Ο(1) rather than the Ο(n) of 'String'.
--
-- This module is a fairly faithful port of Rust's @std::path::PathBuf@ module. Users who
-- want a higher-level API with fancier types can use 'Data.Path'. Users committed to the
-- @streaming@ and @streaming-bytestring@ ecosystem can pull in the @streaming-paths@ package.
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
