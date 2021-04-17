{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Path where

import Data.Coerce
import Data.Kind
import Data.Path.Internal.Last qualified as Sym
import Data.Path.Types hiding (Path (..))
import Data.PathBuf (PathBuf)
import Data.PathBuf qualified as P
import Data.Proxy
import Data.Symbol.Ascii qualified as Sym
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

newtype Path os (ar :: Anchor) (fd :: Entity) = Path {buffer :: PathBuf}

fromBuffer :: PathBuf -> Path os ar fd
fromBuffer = Path

toBuffer :: Path os ar fd -> PathBuf
toBuffer = buffer

(</>) :: Path os ar 'Dir -> Path os 'Rel fd -> Path os ar fd
(</>) = coerce P.join

infixr 5 </>

type PathSeparator = "/"

type Root = "/"

data Result = Ok | MustNotStartWithRoot | MustNotEndWithSeparator | MustStartWithRoot

type family ComputeValidity s ar fd :: Result where
  ComputeValidity s 'Rel 'Dir = If (Sym.Head s == Root) 'MustNotStartWithRoot 'Ok
  ComputeValidity s 'Rel 'File = If (Sym.Head s == Root) 'MustNotStartWithRoot (If (Sym.Last s == PathSeparator) 'MustNotEndWithSeparator 'Ok)
  ComputeValidity s 'Abs 'Dir = If (Not (Sym.Head s == Root)) 'MustStartWithRoot 'Ok
  ComputeValidity s 'Abs 'File = If (Not (Sym.Head s == Root)) 'MustStartWithRoot (If (Sym.Last s == PathSeparator) 'MustNotEndWithSeparator 'Ok)

type family Message (p :: Symbol) (m :: Symbol) :: Constraint where
  Message p m = TypeError ('Text "The path '" ':<>: 'Text p ':<>: 'Text "' must " ':<>: 'Text m)

type family IsValid (s :: Symbol) (r :: Result) :: Constraint where
  IsValid p 'Ok = ()
  IsValid p 'MustNotStartWithRoot = Message p "not start with a root"
  IsValid p 'MustStartWithRoot = Message p "start with a root"
  IsValid p 'MustNotEndWithSeparator = Message p "not end with a directory separator"

type ValidPath p os ar fd = IsValid p (ComputeValidity p ar fd)

path :: forall s os ar fd. (ValidPath s os ar fd, KnownSymbol s) => Path os ar fd
path = Path (P.fromString (symbolVal (Proxy @s)))

rel :: Path os 'Rel 'Dir
rel = path @"foo"

abs :: Path os 'Rel 'File
abs = path @"rel"
