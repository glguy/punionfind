{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

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

class PersistentArray a e s | a -> e s where
  newArr :: Int -> (Int -> e) -> ST s a
  getArr :: a -> Int -> ST s e
  setArr :: a -> Int -> e -> ST s a

modifyArr :: PersistentArray a e s => a -> Int -> (e -> e) -> ST s a
modifyArr a i f = setArr a i . f =<< getArr a i
