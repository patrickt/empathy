module Data.Path.System.Windows
  ( System
  , pathSeparator
  ) where

data Windows
type System = Windows

pathSeparator :: Char
pathSeparator = '\\' -- smh

forbiddenCharacters :: String
forbiddenCharacters = "\\\0*?"
