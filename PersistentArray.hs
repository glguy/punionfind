{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module PersistentArray where

class Monad m => PersistentArray a e m | a -> e m where
  newArr :: Int -> (Int -> e) -> m a 
  getArr :: a -> Int -> m e
  setArr :: a -> Int -> e -> m a

modifyArr :: PersistentArray a e m => a -> Int -> (e -> e) -> m a
modifyArr a i f = setArr a i . f =<< getArr a i
