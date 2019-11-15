module Data.Path.System.Posix
  ( System
  , pathSeparator
  , forbiddenCharacters
  ) where

data Posix
type System = Posix

pathSeparator :: Char
pathSeparator = '/'

forbiddenCharacters :: String
forbiddenCharacters = "/:\0"
