{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

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

newtype M s a = M (StateT (UnionFind RevertArray s) (ChoiceT (ST s)) a)
  deriving (Monad, MonadPlus)

instance BaseM (ST s) (ST s) where
    inBase = id

instance BaseM (M s) (ST s) where
    inBase = M . inBase

-- | Runs the backtracking computation with union/find operations supported
--   on indexes from 0 to n-1 and returns the first success if one is found.
runM :: Int -> M s a -> ST s (Maybe a)
runM n (M m) = do
    uf <- newUnionFind n
    findOne $ fmap fst $ runStateT uf m

unify :: Int -> Int -> M s ()
unify x y = M $ do
    uf <- get
    set =<< inBase (union uf x y)

testEquiv :: Int -> Int -> M s Bool
testEquiv x y = M $ do
    uf <- get
    inBase $ liftM2 (==) (find uf x) (find uf y)
