{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances #-}
module Ref (Ref(..)) where

import Data.STRef
import Control.Monad.ST (ST)

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
