{-# LANGUAGE FlexibleContexts, GADTs #-}

{- |
Module      :  UnionFind
Description :  Efficient backtracking union-find data structure
Copyright   :  (c) 2008 Eric Mertens
License     :  BSD3

Maintainer  :  emertens@gmail.com
Stability   :  provisional
Portability :  non-portable

-}

module UnionFind (UnionFind, newUnionFind, union, find) where

import Control.Monad

import PersistentArray
import Ref
import RevertArray

data UnionFind a m where
   UF :: (Ref ref m, PersistentArray a Int m)
      => ref a -> a -> UnionFind a m

newUnionFind :: (Ref ref m, PersistentArray a Int m)
             => Int -> m (UnionFind a m)
newUnionFind n = liftM2 UF (newRef =<< newArr n id) (newArr n (const 0))

find :: UnionFind a m -> Int -> m Int
find (UF pref _) i = modifyRefM pref (go i)
  where
  go i parents = do
    p <- getArr parents i
    if p == i then return (parents, p) else do
      (parents, p) <- go p parents
      parents      <- setArr parents i p
      return (parents, p)

union :: Monad m => UnionFind a m -> Int -> Int -> m (UnionFind a m)
union uf x y = do
  cx <- find uf x
  cy <- find uf y
  if cx == cy then return uf else do
    rx <- rank uf cx
    ry <- rank uf cy
    case compare rx ry of
      GT -> link uf cy cx
      LT -> link uf cx cy
      EQ -> do uf <- incRank uf cx
               link uf cy cx

rank :: UnionFind a m -> Int -> m Int
rank (UF _ ranks) x = getArr ranks x

link :: UnionFind a m -> Int -> Int -> m (UnionFind a m)
link (UF pref rref) x y = do
  parents <- getRef pref
  pref'   <- newRef =<< setArr parents y x
  return (UF pref' rref)

incRank :: UnionFind a m -> Int -> m (UnionFind a m)
incRank (UF pref ranks) x = UF pref `liftM` modifyArr ranks x (+1)
