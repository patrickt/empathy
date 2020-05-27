{-# LANGUAGE DataKinds #-}
module Data.Path.System.Posix
  ( System
  , PathSeparator
  , forbiddenCharacters
  ) where

data Posix
type System = Posix

type PathSeparator = "/"

forbiddenCharacters :: String
forbiddenCharacters = "/:\0"
