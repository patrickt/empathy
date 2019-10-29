{-# LANGUAGE DataKinds #-}

module Data.Path.Generic
  ( System
  , Path
  , Relative (..)
  , Entity (..)
  , relFile
  ) where

import Data.Path.System
import Data.Path.Types

relFile :: String -> Path System 'Rel 'File
relFile = undefined
