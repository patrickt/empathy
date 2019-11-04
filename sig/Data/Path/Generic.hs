{-# LANGUAGE DataKinds, GADTs, LambdaCase, ScopedTypeVariables, TypeApplications, RankNTypes #-}

-- | You probably don't need to import this module directly. If you're working with cross-platform paths, use "Data.Path.Types"; if you're working with your current system path, use "Data.Path".
module Data.Path.Generic
  ( System
  , Path
  , Anchor (..)
  , Entity (..)
  -- * Eliminating paths
  , toString
  , chooseAbsRel
  , chooseFileDir
  , choose
  -- * Predefined constants
  , currentDir
  , rootDir
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

toString :: Path ar fd -> String
toString p = T.fold mempty (showChar pathSeparator) showString (\a b -> a <> showChar pathSeparator <> b) p ""

chooseAbsRel :: forall ar fd a . AbsRel ar
             => (Path 'Abs fd -> a)
             -> (Path 'Rel fd -> a)
             -> Path ar fd
             -> a
chooseAbsRel onAbs onRel p = case arSing @ar of
  SAbs -> onAbs p
  SRel -> onRel p

chooseFileDir :: forall ar fd a . FileDir fd
             => (Path ar 'File -> a)
             -> (Path ar 'Dir  -> a)
             -> Path ar fd
             -> a
chooseFileDir onFile onDir p = case fdSing @fd of
  SFile -> onFile p
  SDir  -> onDir p

choose :: forall ar fd a . (AbsRel ar, FileDir fd)
       => (Path 'Abs 'File -> a)
       -> (Path 'Rel 'File -> a)
       -> (Path 'Abs 'Dir  -> a)
       -> (Path 'Rel 'Dir  -> a)
       -> Path ar fd
       -> a
choose onAF onRF onAD onRD p = case (arSing @ar, fdSing @fd) of
  (SAbs, SFile) -> onAF p
  (SRel, SFile) -> onRF p
  (SAbs, SDir)  -> onAD p
  (SRel, SDir)  -> onRD p

currentDir :: Path 'Rel 'Dir
currentDir = Cwd

rootDir :: Path 'Abs 'Dir
rootDir = Root

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

