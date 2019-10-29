module Data.Path.System.Windows
  ( System
  , pathSeparator
  , toString
  ) where

import Data.Path.Types

data Windows
type System = Windows

pathSeparator :: Char
pathSeparator = '\\' -- smh

toString :: Path Windows os ar -> String
toString _ = "TODO"
