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
  getArr :: a s -> Int -> ST s Int
  setArr :: a s -> Int -> Int -> ST s (a s)

modifyArr :: PersistentArray a => a s -> Int -> (Int -> Int) -> ST s (a s)
modifyArr a i f = setArr a i . f =<< getArr a i
