{-# LANGUAGE DataKinds #-}

module Data.Path.System.Posix
  ( System
  , pathSeparator
  , toString
  ) where

import Data.Path.Types

data Posix
type System = Posix

pathSeparator :: Char
pathSeparator = '/'

toString :: Path System os ar -> String
toString _ = "TODO"
