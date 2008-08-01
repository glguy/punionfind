{-# LANGUAGE FlexibleContexts, GADTs #-}

module UnionFind (UnionFind, newUnionFind, union, find) where

import Control.Monad
import Control.Monad.ST
import Data.STRef

import PersistentArray
import Ref
import RevertArray


data UnionFind a s where
   UF :: PersistentArray (a Int s) Int (ST s)
      => STRef s (a Int s) -> a Int s -> UnionFind a s

newUnionFind :: PersistentArray (a Int s) Int (ST s)
             => Int -> ST s (UnionFind a s)
newUnionFind n = liftM2 UF (newRef =<< newArr n id) (newArr n (const 0))

find :: UnionFind a s -> Int -> ST s Int
find (UF pref _) i = modifyRefM pref (go i)
  where
  go i parents = do
    p <- getArr parents i
    if p == i then return (parents, p) else do
      (parents, p) <- go p parents
      parents      <- setArr parents i p
      return (parents, p)

union :: UnionFind a s -> Int -> Int -> ST s (UnionFind a s)
union uf x y = do
  cx <- find uf x
  cy <- find uf y
  if cx == cy then return uf else do
    rx <- rank uf x
    ry <- rank uf y
    case compare rx ry of
      GT -> link uf cy cx
      LT -> link uf cx cy
      EQ -> do uf <- incRank uf cx
               link uf cy cx

rank :: UnionFind a s -> Int -> ST s Int
rank (UF _ ranks) x = getArr ranks x

link :: UnionFind a s -> Int -> Int -> ST s (UnionFind a s)
link (UF pref rref) x y = do
  parents <- getRef pref
  pref'   <- newRef =<< setArr parents y x
  return (UF pref' rref)

incRank :: UnionFind a s -> Int -> ST s (UnionFind a s)
incRank (UF pref ranks) x = UF pref `fmap` modifyArr ranks x (+1)
