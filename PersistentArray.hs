{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module PersistentArray (PersistentArray(..)) where

import Control.Monad.ST (ST)

class PersistentArray a e s | a -> e s where
  create :: Int -> (Int -> e) -> ST s a
  get    :: a -> Int -> ST s e
  set    :: a -> Int -> e -> ST s a
