{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Data.Path.Types
    ( Path (..)
    , Relative (..)
    , Entity (..)
    , OS (..)
    , (</>)
    ) where

data Relative = Abs | Rel | AbsRel

data Entity = File | Dir | FileDir

data OS = Posix | Windows

data Path (os :: OS) (ar :: Relative) (fd :: Entity) where
  Cwd   :: Path os 'Rel 'Dir
  Root  :: Path os 'Abs 'Dir
  Slash :: Path os ar 'Dir -> Path os 'Rel fd -> Path os ar fd

(</>) :: Path os ar 'Dir -> Path os 'Rel fd -> Path os ar fd
(</>) = Slash
