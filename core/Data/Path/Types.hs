{-# LANGUAGE DataKinds, GADTs, KindSignatures, LambdaCase, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}

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
import GHC.TypeLits

data Anchor = Abs | Rel deriving (Show, Eq)

data SAnchor (ar :: Anchor) where
  SAbs :: SAnchor 'Abs
  SRel :: SAnchor 'Rel

class AbsRel (ar :: Anchor) where
  arSing :: SAnchor ar

instance AbsRel 'Abs where arSing = SAbs
instance AbsRel 'Rel where arSing = SRel

data Entity = File | Dir deriving (Show, Eq)

data SEntity (fd :: Entity) where
  SFile :: SEntity 'File
  SDir  :: SEntity 'Dir

class FileDir (fd :: Entity) where
  fdSing :: SEntity fd

instance FileDir 'File where fdSing = SFile
instance FileDir 'Dir  where fdSing = SDir

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

deriving instance Show    (Path os ar fd)
deriving instance Eq      (Path os ar fd)

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
