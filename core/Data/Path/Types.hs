{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Data.Path.Types
    ( Path (..)
    , Relative (..)
    , Entity (..)
    , (</>)
    ) where

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
  Slash ::
    Path os ar 'Dir
    -> Path os 'Rel fd
    -> Path os ar fd

(</>) :: Path os ar 'Dir -> Path os 'Rel fd -> Path os ar fd
(</>) = Slash
