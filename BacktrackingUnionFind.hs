{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}
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
import MonadLib.Derive
import Control.Monad
import Control.Monad.ST

import UnionFind
import RevertArray
import Data.IntMap (IntMap)

newtype M s a = M
  { unM :: StateT (UnionFind (RevertArray Int s) s) (ChoiceT (ST s)) a }

iso = Iso M unM

instance BaseM (M s) (ST s) where
    inBase  = derive_inBase iso
    
instance Monad (M s) where
    (>>=)   = derive_bind iso
    return  = derive_return iso
    fail    = derive_fail iso

instance MonadPlus (M s) where
    mplus   = derive_mplus iso
    mzero   = derive_mzero iso

-- | Runs the backtracking computation with union/find operations supported
--   on indexes from 0 to n-1 and returns the first success if one is found.
runM :: Int -> (forall s. M s b) -> [b]
runM n m = runST (do uf <- newUnionFind n
                     findAll $ fmap fst $ runStateT uf $ unM m)

unify :: Int -> Int -> M s ()
unify x y = M $ do
    uf <- get
    set =<< inBase (union uf x y)

testEquiv :: Int -> Int -> M s Bool
testEquiv x y = M $ do
    uf <- get
    inBase $ liftM2 (==) (find uf x) (find uf y)
