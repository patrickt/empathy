{-# LANGUAGE DataKinds, GADTs, KindSignatures, LambdaCase, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}

module Data.Path.Types
    ( Path (..)
    , Relative (..)
    , Entity (..)
    , fold
    ) where

import Data.Hashable
import Data.String
import GHC.TypeLits

data Relative = Abs | Rel deriving (Show, Eq)

data Entity = File | Dir deriving (Show, Eq)

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
