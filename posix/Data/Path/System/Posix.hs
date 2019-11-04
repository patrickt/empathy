module Data.Path.System.Posix
  ( System
  , pathSeparator
  ) where

data Posix
type System = Posix

pathSeparator :: Char
pathSeparator = '/'

forbiddenCharacters :: String
forbiddenCharacters = ":/\0"
