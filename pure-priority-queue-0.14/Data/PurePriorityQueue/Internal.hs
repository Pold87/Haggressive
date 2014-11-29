{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}
------------------------------------------------------------
-- |
-- Module       : Data.PurePriorityQueue.Internal
-- Copyright    : (c) 2009 Bradford Larsen
-- License      : BSD-style
-- Maintainer   : brad.larsen@gmail.com
--
-- This module exposes the internals of a pure priority queue,
-- implemented on top of "Data.Map".
--
-- Estimates of worst-case time complexity are given.  The value /n/
-- is the number of elements in the queue.  The value /p/ is the
-- cardinality of the set of priorities of the elements in the queue.
-- /p/ is never greater than /n/.
------------------------------------------------------------

module Data.PurePriorityQueue.Internal where

import qualified Data.Map as M
import qualified Data.Foldable as F
import Data.Monoid

import Prelude hiding (filter, null)
import qualified Prelude


-- | A queue of values of type 'a' with priority of type 'p'.
newtype MinMaxQueue p a = MinMaxQueue { unMinMaxQueue :: M.Map p [a] }
  deriving (Eq, Ord)

-- | /O(1)/ An empty priority queue.
empty :: MinMaxQueue p a
empty = MinMaxQueue M.empty
{-# INLINE empty #-}

-- | /O(1)/ Test whether a priority queue is empty.
null :: MinMaxQueue p a -> Bool
null = M.null . unMinMaxQueue
{-# INLINE null #-}

-- | /O(1)/ A priority queue with a single entry.
singleton :: Ord p => a -> p -> MinMaxQueue p a
singleton a p = insert a p empty


-- | /O(log p)/ Insert a value with given priority into a priority queue.
insert :: Ord p
       => a
       -> p
       -> MinMaxQueue p a
       -> MinMaxQueue p a
insert a p (MinMaxQueue m) = MinMaxQueue (M.insertWith' (++) p [a] m)
{-# INLINE insert #-}

-- | /O(log p)/ Remove the value with the minimum priority from the
-- queue.
--
-- If the queue is empty, 'deleteMin' returns 'empty'.  Ties are
-- broken arbitrarily.
deleteMin :: Ord p
          => MinMaxQueue p a
          -> MinMaxQueue p a
deleteMin m = maybe empty snd (minView m)
{-# INLINE deleteMin #-}

-- | /O(log p)/ Remove the value with the maximum priority from the
-- queue.
--
-- If the queue is empty, 'deleteMax' returns 'empty'.  Ties are
-- broken arbitrarily.
deleteMax :: Ord p
          => MinMaxQueue p a
          -> MinMaxQueue p a
deleteMax m = maybe empty snd (maxView m)
{-# INLINE deleteMax #-}


-- | Applies a 'Data.Map.Map' view function to a given priority queue.
viewWith :: (Ord p)
         => (M.Map p [a] -> Maybe ((p, [a]), M.Map p [a]))  -- ^ The view function
         -> MinMaxQueue p a                                 -- ^ The priority queue
         -> Maybe ((a, p), MinMaxQueue p a)
viewWith f (MinMaxQueue m) = do
  ((p, a:as), m') <- f m
  let m'' = if Prelude.null as
            then m'
            else M.insert p as m'
  return ((a, p), MinMaxQueue m'')
{-# INLINE viewWith #-}

-- | /O(log p)/ View a priority queue to get the (value, priority)
-- pair with the lowest priority and the remainder of the queue.
--
-- Ties are broken arbitrarily.
minView :: Ord p
        => MinMaxQueue p a
        -> Maybe ((a, p), MinMaxQueue p a)
minView = viewWith M.minViewWithKey
{-# INLINE minView #-}

-- | /O(log p)/ View a priority queue to get the (value, priority)
-- pair with the highest priority and the remainder of the queue.
--
-- Ties are broken arbitrarily.
maxView :: Ord p => MinMaxQueue p a -> Maybe ((a, p), MinMaxQueue p a)
maxView = viewWith M.maxViewWithKey
{-# INLINE maxView #-}

-- | /O(log p)/ Get the minimum priority of the elements in the queue.
minPriority :: Ord p => MinMaxQueue p a -> Maybe p
minPriority = fmap (snd . fst) . minView
{-# INLINE minPriority #-}

-- | /O(log p)/ Get the maximum priority of the elements in the queue.
maxPriority :: Ord p => MinMaxQueue p a -> Maybe p
maxPriority = fmap (snd . fst) . maxView
{-# INLINE maxPriority #-}

-- | /O(n)/ Fold the priorities and values of a priority queue.
foldWithPriority :: Ord p => (p -> a -> b -> b) -> b -> MinMaxQueue p a -> b
foldWithPriority f s q = M.foldWithKey f' s (unMinMaxQueue q)
  where f' !p !vs !acc = foldr (f p) acc vs
{-# INLINE foldWithPriority #-}

-- | /O(log p)/ Split a priority queue 'q' into two queues @(q1, q2)@
-- by the given priority 'p', such that 'q1' contains exactly the
-- entries with priority less than 'p', and 'q2' containes exactly the
-- entries with priority greater than or equal to 'p'.
splitByPriority :: Ord p => p -> MinMaxQueue p a -> (MinMaxQueue p a, MinMaxQueue p a)
splitByPriority p q = (MinMaxQueue lt, MinMaxQueue geq)
  where
    geq = case meq of
            Nothing -> gt
            Just eq -> M.insert p eq gt
    (lt, meq, gt) = M.splitLookup p (unMinMaxQueue q)
{-# INLINE splitByPriority #-}

-- | /O(n)/ The number of entries in a priority queue.
size :: Ord p => MinMaxQueue p a -> Int
size m = getSum $ F.foldMap (const $ Sum 1) m
{-# INLINE size #-}

-- TODO:  filter could be implemented smarter.  Perhaps
-- filterWithPriority also.

-- | /O(n log p)/ Filter all values that satisfy the predicate.
filter :: (Ord p) => (a -> Bool) -> MinMaxQueue p a -> MinMaxQueue p a
filter f = filterWithPriority (\k _ -> f k)
{-# INLINE filter #-}

-- | /O(n log p)/ Filter all entries that satisfy the predicate.
filterWithPriority :: (Ord p)
                   => (a -> p -> Bool)
                   -> MinMaxQueue p a
                   -> MinMaxQueue p a
filterWithPriority f = foldWithPriority f' empty
  where
    f' !p !k !q = if f k p then insert k p q else q
{-# INLINE filterWithPriority #-}

-- | /O(n)/ Convert the priority queue into a list of (value,
-- priority) pairs in ascending priority.
--
-- Ties are broken in an arbitrary manner.
toAscList :: (Ord p) => MinMaxQueue p a -> [(a, p)]
toAscList = foldWithPriority (\p a vs -> (a, p) : vs) []
{-# INLINE toAscList #-}


instance Functor (MinMaxQueue p) where
  fmap f q = MinMaxQueue (fmap (fmap f) $ unMinMaxQueue q)
  {-# INLINE fmap #-}

instance F.Foldable (MinMaxQueue p) where
  foldMap f q = F.foldMap (F.foldMap f) (unMinMaxQueue q)
  {-# INLINE foldMap #-}

instance Ord p => Monoid (MinMaxQueue p a) where
  mempty = empty
  mappend q1 q2 = MinMaxQueue $ M.unionWith (++) q1' q2'
    where
      q1' = unMinMaxQueue q1
      q2' = unMinMaxQueue q2
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
