{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, Rank2Types #-}
{- |
Module      :  BacktrackingUnionFind
Description :  Efficient backtracking union-find monad
Copyright   :  (c) 2008 Eric Mertens
License     :  BSD3

Maintainer  :  emertens@gmail.com
Stability   :  provisional
Portability :  non-portable

-}

module BacktrackingUnionFind (M, runM, unify, testEquiv) where

import MonadLib
import Control.Monad
import Control.Monad.ST

import UnionFind
import RevertArray
import MapArray
import Data.IntMap (IntMap)

newtype M s a = M
  { unM :: StateT (UnionFind (RevertArray Int s) (ST s)) (ChoiceT (ST s)) a }
  deriving (Monad, MonadPlus)

instance BaseM (ST s) (ST s) where
    inBase = id

instance BaseM (M s) (ST s) where
    inBase = M . inBase

-- | Runs the backtracking computation with union/find operations supported
--   on indexes from 0 to n-1 and returns the first success if one is found.
runM :: forall b. Int -> (forall s. M s b) -> [b]
runM n m = runST $ do uf <- newUnionFind n
                      findAll $ fmap fst $ runStateT uf $ unM m

unify :: Int -> Int -> M s ()
unify x y = M $ do
    uf <- get
    set =<< inBase (union uf x y)

testEquiv :: Int -> Int -> M s Bool
testEquiv x y = M $ do
    uf <- get
    inBase $ liftM2 (==) (find uf x) (find uf y)
