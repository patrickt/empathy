{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | You probably don't need to import this module directly. If you're working with cross-platform paths, use "Data.Path.Types"; if you're working with your current system path, use "Data.Path".
--
-- This module is meant to be imported qualified, e.g.:
--
-- @
--  import Data.Path (Path)
--  import qualified Data.Path as Path
-- @
module Data.Path.Generic
  ( System,
    Path,
    Anchor (..),
    Entity (..),
    AbsRel,
    FileDir,
    IsRelative,
    IsAbsolute,

    -- * Constructing paths
    parse,
    currentDir,
    rootDir,

    -- * Unsafe path constructors
    relDir,

    -- * Eliminating paths
    toString,
    chooseAbsRel,
    chooseFileDir,
    choose,

    -- * Building paths
    combine,
    (</>),
    addExtension,
    (<.>),
  )
where

import Control.Applicative (some)
import Control.Monad (void)
import Data.Aeson
import Data.Bifunctor (first)
import Data.Coerce
import Data.Kind
import Data.Path.System
import Data.Path.Types hiding (Path)
import qualified Data.Path.Types as T
import Data.Proxy (Proxy (..))
import qualified Data.Symbol.Ascii as Sym
import Data.Type.Equality
import Data.Void (Void)
import GHC.TypeLits
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P

type family Ensure (ar :: Anchor) (a :: Bool) :: Constraint where
  Ensure 'Abs 'True = ()
  Ensure 'Abs _ = TypeError ('Text "Absolute paths must begin with a slash")
  Ensure 'Rel 'False = ()
  Ensure 'Rel _ = TypeError ('Text "Relative paths must not begin with a slash")

class KnownSymbol s => IsAbsolute (s :: Symbol)

instance (KnownSymbol s, Ensure 'Abs (Sym.Head s == PathSeparator)) => IsAbsolute s

class KnownSymbol s => IsRelative (s :: Symbol)

instance (KnownSymbol s, Ensure 'Rel (Sym.Head s == PathSeparator)) => IsRelative s

-- * Constructing paths

type Path = T.Path System

instance (AbsRel ar, FileDir fd) => FromJSON (Path ar fd) where
  parseJSON v = do
    str <- parseJSON @String v
    either fail pure $ parse str

-- | The current directory. You can use this with '</>' to build relative paths.
currentDir :: Path 'Rel 'Dir
currentDir = T.fromString "."

-- | The root directory.
rootDir :: Path 'Abs 'Dir
rootDir = T.Path "/"

-- | Construct a relative directory from a type-level string. To invoke
-- this function, pass it an explicit type application of a string:
--
-- > rootDir </> relDir @"tmp"
--
-- This currently performs no compile-time parsing of the string; this behavior
-- may change if GHC makes doing so easier. For now, it calls 'error'.
relDir :: forall str. IsRelative str => Path 'Rel 'Dir
relDir = T.Path (symbolVal (Proxy @str))

parse ::
  forall ar fd.
  (AbsRel ar, FileDir fd) =>
  String ->
  Either String (Path ar fd)
parse str = first P.errorBundlePretty $ P.parse @_ @String (parser <* P.eof) "" str
  where
    separator = P.string (symbolVal (Proxy @PathSeparator))
    parser :: P.Parsec Void String (Path ar fd)
    parser =
      case (testEquality (arSing @ar) SAbs, testEquality (arSing @ar) SRel, testEquality (fdSing @fd) SDir, testEquality (fdSing @fd) SFile) of
        (isAbs, isRel, isDir, isFile) -> do
          case isAbs of
            Just Refl -> void separator
            Nothing -> pure ()
          let separatedBy = case isDir of
                Just Refl -> P.sepEndBy
                Nothing -> P.sepBy
          comps <- some (P.noneOf forbiddenCharacters) `separatedBy` some separator
          case (isAbs, isRel, isDir, isFile) of
            -- Absolute directory
            (Just Refl, Nothing, Just Refl, Nothing) -> pure (T.Path str)
            -- Relative directory
            (Nothing, Just Refl, Just Refl, Nothing) -> pure (T.Path str)
            -- Absolute file
            (Just Refl, Nothing, Nothing, Just Refl)
              | null comps -> fail "Expected one or more path components"
              | otherwise -> pure (T.Path str)
            -- Relative file
            (Nothing, Just Refl, Nothing, Just Refl)
              | null comps -> fail "Expected one or more path components"
              | otherwise -> pure (T.Path str)
            -- Unreachable
            _ -> fail "Failure of `empathy`: this should be unreachable"

-- * Eliminating paths

-- | An eliminator for absolute or relative paths, ignoring entity type.
chooseAbsRel ::
  forall ar fd a.
  AbsRel ar =>
  -- | Handles absolute paths.
  (Path 'Abs fd -> a) ->
  -- | Handles relative paths.
  (Path 'Rel fd -> a) ->
  -- | The path to analyze.
  Path ar fd ->
  a
chooseAbsRel onAbs onRel p = case arSing @ar of
  SAbs -> onAbs p
  SRel -> onRel p

-- | An eliminator for files or directories, ignoring anchor status.
chooseFileDir ::
  forall ar fd a.
  FileDir fd =>
  -- | Handles files.
  (Path ar 'File -> a) ->
  -- | Handles directories.
  (Path ar 'Dir -> a) ->
  -- | The path to analyze.
  Path ar fd ->
  a
chooseFileDir onFile onDir p = case fdSing @fd of
  SFile -> onFile p
  SDir -> onDir p

-- | A general-purpose eliminator for any 'Path' type.
choose ::
  forall ar fd a.
  (AbsRel ar, FileDir fd) =>
  -- | Handles absolute files.
  (Path 'Abs 'File -> a) ->
  -- | Handles relative files.
  (Path 'Rel 'File -> a) ->
  -- | Handles absolute directories.
  (Path 'Abs 'Dir -> a) ->
  -- | Handles relative directories.
  (Path 'Rel 'Dir -> a) ->
  -- | The path to analyze.
  Path ar fd ->
  a
choose onAF onRF onAD onRD p = case (arSing @ar, fdSing @fd) of
  (SAbs, SFile) -> onAF p
  (SRel, SFile) -> onRF p
  (SAbs, SDir) -> onAD p
  (SRel, SDir) -> onRD p

-- * Building paths

-- | Join a directory (relative or absolute) with a relative component (file or directory).
combine :: Path ar 'Dir -> Path 'Rel fd -> Path ar fd
combine = coerce go
  where
    sep = symbolVal (Proxy @PathSeparator)
    go a b
      | a == sep = a <> b
      | otherwise = a <> sep <> b

infixr 5 </>

-- | Infix variant of 'combine'.
(</>) :: Path ar 'Dir -> Path 'Rel fd -> Path ar fd
(</>) = combine
{-# INLINE (</>) #-}

-- | Add an extension to a file.
addExtension :: Path ar 'File -> String -> Path ar 'File
addExtension = coerce go
  where
    go :: String -> String -> String
    go x y = x <> "." <> y

-- | Infix variant of 'addExtension'.
(<.>) :: Path ar 'File -> String -> Path ar 'File
(<.>) = addExtension
