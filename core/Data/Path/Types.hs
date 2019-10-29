{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Data.Path.Types
    ( Path (..)
    , Relative (..)
    , Entity (..)
    , (</>)
    , combine
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
  Combine ::
    Path os ar 'Dir
    -> Path os 'Rel fd
    -> Path os ar fd

-- | Join a directory (relative or absolute) with a relative component (file or directory).
combine :: Path os ar 'Dir -> Path os 'Rel fd -> Path os ar fd
combine = Combine
{-# INLINE combine #-}

-- | Infix variant of 'combine'.
(</>) :: Path os ar 'Dir -> Path os 'Rel fd -> Path os ar fd
(</>) = combine
{-# INLINE (</>) #-}
