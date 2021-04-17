{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
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
