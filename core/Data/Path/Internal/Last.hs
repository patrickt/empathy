{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Path.Internal.Last (Last) where

import Data.Symbol.Ascii qualified as Sym

type (:<) = '(:)
type Ø    = '[]

type family LLast (as :: [k]) :: k where
  LLast (a :< as) = Last' a as

type family Last' (a :: k) (as :: [k]) :: k where
  Last' a Ø         = a
  Last' a (b :< as) = Last' b as

-- this is so slow.
type Last s = LLast (Sym.ToList s)
