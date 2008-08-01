{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}

{- |
Module      :  Ref
Description :  Generic operations on reference types
Copyright   :  (c) 2008 Eric Mertens
License     :  BSD3

Maintainer  :  emertens@gmail.com
Stability   :  provisional
Portability :  non-portable

-}

module Ref where

import Data.IORef
import Data.STRef
import Control.Monad.ST

class Monad m => Ref ref m | ref -> m where
  newRef :: a -> m (ref a)
  setRef :: ref a -> a -> m ()
  getRef :: ref a -> m a

  modifyRef :: ref a -> (a -> a) -> m ()
  modifyRef ref f = setRef ref . f =<< getRef ref

  modifyRefM :: ref a -> (a -> m (a,b)) -> m b
  modifyRefM ref f = do
    (x',res) <- f =<< getRef ref
    setRef ref x'
    return res

instance Ref (STRef s) (ST s) where
  newRef = newSTRef
  setRef = writeSTRef
  getRef = readSTRef

instance Ref IORef IO where
  newRef = newIORef
  setRef = writeIORef
  getRef = readIORef
