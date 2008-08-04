{- |
Module      :  PersistentArray
Description :  Generic operations on immutable arrays in a monad
Copyright   :  (c) 2008 Eric Mertens
License     :  BSD3

Maintainer  :  emertens@gmail.com
Stability   :  provisional
Portability :  non-portable

-}

module PersistentArray where

import Control.Monad.ST

class PersistentArray a where
  newArr :: Int -> (Int -> Int) -> ST s (a s)
  getArr :: Int -> a s -> ST s Int
  setArr :: Int -> Int -> a s -> ST s (a s)
