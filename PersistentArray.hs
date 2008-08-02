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

class Monad m => PersistentArray a e m | a -> e m where
  newArr :: Int -> (Int -> e) -> m a
  getArr :: a -> Int -> m e
  setArr :: a -> Int -> e -> m a

modifyArr :: PersistentArray a e m => a -> Int -> (e -> e) -> m a
modifyArr a i f = setArr a i . f =<< getArr a i
