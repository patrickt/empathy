{-# LANGUAGE DataKinds, GADTs, LambdaCase, ScopedTypeVariables, TypeApplications, RankNTypes #-}

-- | You probably don't need to import this module directly. If you're working with cross-platform paths, use "Data.Path.Types"; if you're working with your current system path, use "Data.Path".
--
-- This module is meant to be imported qualified, e.g.:
--
-- @
--  import Data.Path (Path)
--  import qualified Data.Path as Path
-- @
module Data.Path.Generic
  ( System
  , Path
  , Anchor (..)
  , Entity (..)
  -- * Constructing
  , currentDir
  , rootDir
  -- * Eliminating paths
  , toString
  , chooseAbsRel
  , chooseFileDir
  , choose
  -- * Building paths
  , combine
  , (</>)
  , addExtension
  , (<.>)
  ) where

import Data.Path.System
import Data.Path.Types hiding (Path)
import qualified Data.Path.Types as T

type Path = T.Path System

-- | The current directory. You can use this with '</>' to build relative paths.
currentDir :: Path 'Rel 'Dir
currentDir = Cwd

-- | The root directory.
rootDir :: Path 'Abs 'Dir
rootDir = Root

parse :: forall ar fd . String -> Maybe (Path ar fd)
parse = undefined

-- | Convert a 'Path' to a String, suitable for being passed as a @FilePath@ from @System.FilePath@.
toString :: Path ar fd -> String
toString p = T.fold mempty (showChar pathSeparator) showString (\a b -> a <> showChar pathSeparator <> b) p ""

-- | An eliminator for absolute or relative paths, ignoring entity type.
chooseAbsRel :: forall ar fd a . AbsRel ar
             => (Path 'Abs fd -> a) -- ^ Handles absolute paths.
             -> (Path 'Rel fd -> a) -- ^ Handles relative paths.
             -> Path ar fd          -- ^ The path to analyze.
             -> a
chooseAbsRel onAbs onRel p = case arSing @ar of
  SAbs -> onAbs p
  SRel -> onRel p

-- | An eliminator for files or directories, ignoring anchor status.
chooseFileDir :: forall ar fd a . FileDir fd
             => (Path ar 'File -> a) -- ^ Handles files.
             -> (Path ar 'Dir  -> a) -- ^ Handles directories.
             -> Path ar fd           -- ^ The path to analyze.
             -> a
chooseFileDir onFile onDir p = case fdSing @fd of
  SFile -> onFile p
  SDir  -> onDir p

-- | A general-purpose eliminator for any 'Path' type.
choose :: forall ar fd a . (AbsRel ar, FileDir fd)
       => (Path 'Abs 'File -> a) -- ^ Handles absolute files.
       -> (Path 'Rel 'File -> a) -- ^ Handles relative files.
       -> (Path 'Abs 'Dir  -> a) -- ^ Handles absolute directories.
       -> (Path 'Rel 'Dir  -> a) -- ^ Handles relative directories.
       -> Path ar fd             -- ^ The path to analyze.
       -> a
choose onAF onRF onAD onRD p = case (arSing @ar, fdSing @fd) of
  (SAbs, SFile) -> onAF p
  (SRel, SFile) -> onRF p
  (SAbs, SDir)  -> onAD p
  (SRel, SDir)  -> onRD p

-- | Join a directory (relative or absolute) with a relative component (file or directory).
combine :: Path ar 'Dir -> Path 'Rel fd -> Path ar fd
combine = Combine
{-# INLINE combine #-}

infixr 5 </>

-- | Infix variant of 'combine'.
(</>) :: Path ar 'Dir -> Path 'Rel fd -> Path ar fd
(</>) = combine
{-# INLINE (</>) #-}

-- | Add an extension to a file.
addExtension :: Path ar 'File -> String -> Path ar 'File
addExtension path ext =
  let ext' = '.' : dropWhile (== '.') ext
  in case path of
    Comp s -> Comp (s <> ext')
    Combine xs as -> Combine xs (addExtension as ext)

-- | Infix variant of 'addExtension'.
(<.>) :: Path ar 'File -> String -> Path ar 'File
(<.>) = addExtension

