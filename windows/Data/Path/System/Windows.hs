{-# LANGUAGE DataKinds #-}
module Data.Path.System.Windows
  ( System,
    PathSeparator,
    forbiddenCharacters,
  )
where

data Windows

type System = Windows

type PathSeparator = "\\"

forbiddenCharacters :: String
forbiddenCharacters = "\\\0*?"
