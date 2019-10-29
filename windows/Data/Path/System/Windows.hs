module Data.Path.System.Windows
  ( System
  , pathSeparator
  ) where

import Data.Path.Types

data Windows
type System = Windows

pathSeparator :: Char
pathSeparator = '\\' -- smh
