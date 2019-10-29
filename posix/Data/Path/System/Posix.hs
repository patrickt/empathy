module Data.Path.System.Posix
  ( System
  , pathSeparator
  ) where

import Data.Path.Types

data Posix
type System = Posix

pathSeparator :: Char
pathSeparator = '/'
