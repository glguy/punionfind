{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module MapArray () where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import PersistentArray

instance Monad m => PersistentArray (IntMap e) e m where
  newArr n f   = return $ IntMap.fromList [(k,f k) | k <- [0..n-1]]
  getArr m i   = IntMap.lookup i m
  setArr m i e = return $ IntMap.insert i e m
