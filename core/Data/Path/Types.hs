{-# LANGUAGE DataKinds, GADTs, KindSignatures, RankNTypes, TypeOperators, UndecidableInstances #-}

module Data.Path.Types
    ( Path (..)
    , Relative (..)
    , Entity (..)
    , (</>)
    , combine
    ) where

import Data.String
import GHC.TypeLits

data Relative = Abs | Rel

data Entity = File | Dir

data Path os (ar :: Relative) (fd :: Entity) where
  Cwd ::
    Path os 'Rel 'Dir
  Root ::
    Path os 'Abs 'Dir
  Comp ::
    String
    -> Path os ar fd
  Combine ::
    Path os ar 'Dir
    -> Path os 'Rel fd
    -> Path os ar fd

instance TypeError ('Text "Path does not provide an IsString instance."
                    ':$$: 'Text "Use the 'rel' or 'abs' family of functions instead."
                   ) => IsString (Path os ar fd) where
  fromString = error "uninhabited"

-- | Join a directory (relative or absolute) with a relative component (file or directory).
combine :: Path os ar 'Dir -> Path os 'Rel fd -> Path os ar fd
combine = Combine
{-# INLINE combine #-}

-- | Infix variant of 'combine'.
(</>) :: Path os ar 'Dir -> Path os 'Rel fd -> Path os ar fd
(</>) = combine
{-# INLINE (</>) #-}
