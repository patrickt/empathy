{-# LANGUAGE DataKinds, GADTs, KindSignatures, LambdaCase, RankNTypes, StandaloneDeriving, TypeOperators,
             UndecidableInstances #-}

module Data.Path.Types
    ( -- * Core types
      Path (..)
    , Anchor (..)
    , Entity (..)
      -- * Singleton classes and types
    , AbsRel (..)
    , SAnchor (..)
    , FileDir (..)
    , SEntity (..)
    , fold
    ) where

import Data.Hashable
import Data.String
import Data.Type.Equality
import GHC.TypeLits

-- | Indicates whether a 'Path' is absolute or relative.
data Anchor = Abs | Rel deriving (Show, Eq)

-- | The singleton type for 'Anchor' kinds.
data SAnchor (ar :: Anchor) where
  SAbs :: SAnchor 'Abs
  SRel :: SAnchor 'Rel

instance TestEquality SAnchor where
  testEquality SAbs SAbs = Just Refl
  testEquality SRel SRel = Just Refl
  testEquality _    _    = Nothing

-- | Type-level witness for whether a path is absolute or relative.
-- You'll need this if you use any of the @choose@ family of functions.
class AbsRel (ar :: Anchor) where
  arSing :: SAnchor ar

instance AbsRel 'Abs where arSing = SAbs
instance AbsRel 'Rel where arSing = SRel

-- | Indicates whether a 'Path' refers to a file or directory.
data Entity = File | Dir deriving (Show, Eq)

-- | The singleton type for 'Entity' kinds.
data SEntity (fd :: Entity) where
  SFile :: SEntity 'File
  SDir  :: SEntity 'Dir

instance TestEquality SEntity where
  testEquality SFile SFile = Just Refl
  testEquality SDir SDir   = Just Refl
  testEquality _    _      = Nothing


-- | Type-level witness for a path's entity kind, as per 'AbsRel'.
class FileDir (fd :: Entity) where
  fdSing :: SEntity fd

instance FileDir 'File where fdSing = SFile
instance FileDir 'Dir  where fdSing = SDir

-- | The fundamental path type, bearing a phantom @os@ type.
-- You probably shouldn't patterm-match on this directly; the @choose@
-- or @fold@ functions are more useful.
data Path os (ar :: Anchor) (fd :: Entity) where
  Cwd ::
    Path os 'Rel 'Dir
  Root ::
    Path os 'Abs 'Dir
  Comp ::
    String
    -> Path os 'Rel fd
  Combine ::
    Path os ar 'Dir
    -> Path os 'Rel fd
    -> Path os ar fd

deriving instance Show (Path os ar fd)
deriving instance Eq   (Path os ar fd)

-- This looks like it is hardcoded to Unix, and I guess it technically is, but it doesn't matter.
instance Hashable (Path os ar fd) where
  hashWithSalt salt = fold (hashWithSalt salt '.') (hashWithSalt salt '/') (hashWithSalt salt) hashWithSalt

fold :: f
     -> f
     -> (String -> f)
     -> (f -> f -> f)
     -> Path os ar fd
     -> f
fold cwd root comp combine = \case
  Cwd         -> cwd
  Root        -> root
  Comp s      -> comp s
  Combine a b -> combine (fold cwd root comp combine a) (fold cwd root comp combine b)

instance TypeError ('Text "Path does not provide an IsString instance."
                    ':$$: 'Text "Use the 'rel' or 'abs' family of functions instead."
                   ) => IsString (Path os ar fd) where
  fromString = error "uninhabited"
