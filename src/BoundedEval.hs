{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module BoundedEval
  ( boundedEval,
    bvals,
    scan,
    cutoff,
    eval,
    bval,
  ) where

import NumHask.Prelude
import Data.Foldable
import Data.Tree
import GHC.Err (error)

-- Bird & Hughes (1985)
-- https://sci-hub.ee/10.1016/0020-0190(87)90198-0

boundedEval :: (Ord a) => a -> a -> [Tree a] -> a
boundedEval = bvals max min

-- >>> testtree = (Node 3 [Node 3 [Node 3 [Node (- 1) [], Node 3 []], Node 5 [Node 5 [], Node 2 []]], Node (- 4) [Node (- 4) [Node (- 6) [], Node (- 4) []], Node 0 [Node 2 [], Node 1 []]]] :: Tree Double)
-- >>> (Node _ t1) = testtree
-- bvals max min negInfinity infinity t1
-- 3.0
bvals :: Eq t => (t -> t -> t) -> (t -> t -> t) -> t -> t -> [Tree t] -> t
bvals f g a b ts = cutoff b (scan (bval g f b) a ts)

bval :: (t -> t -> t) -> (t -> t -> t) -> t -> t -> Tree t -> t
bval f g a b t = g (f a (eval f g t)) b

scan :: (x -> a -> x) -> x -> [a] -> [x]
scan _ a [] = [a]
scan f a (x:xs) = a: scan f (f a x) xs

cutoff :: Eq a => a -> [a] -> a
cutoff _ [] = error "woops!"
cutoff _ [x] = x
cutoff b (x:xs) = bool (cutoff b xs) b (x==b)

eval :: (a -> a -> a) -> (a -> a -> a) -> Tree a -> a
eval _ _ (Node x []) = x
eval f g (Node _ ts) = foldr1 f (fmap (eval g f) ts)

