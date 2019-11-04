{-# LANGUAGE DataKinds, GADTs, LambdaCase #-}

-- | You probably don't need to import this module directly. If you're working with cross-platform paths, use "Data.Path.Types"; if you're working with your current system path, use "Data.Path".
module Data.Path.Generic
  ( System
  , Path
  , Relative (..)
  , Entity (..)
  -- * Eliminating paths
  , toString
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

