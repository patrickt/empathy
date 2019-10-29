{-# LANGUAGE DataKinds #-}

module Data.Path.Posix where

import Data.Path.Types

type System = 'Posix

pathSeparator :: Char
pathSeparator = '/'
