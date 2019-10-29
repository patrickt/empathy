{-# LANGUAGE DataKinds #-}

-- | You probably don't need to import this module directly. If you're working with cross-platform paths, use 'Data.Path.Types'; if you're working with your current system path, use 'Data.Path'.
module Data.Path.Generic
  ( System
  , Path
  , Relative (..)
  , Entity (..)
  , combine
  , (</>)
  ) where

import Data.Path.System
import Data.Path.Types

relFile :: String -> Path System 'Rel 'File
relFile = undefined
