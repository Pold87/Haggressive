{-# OPTIONS -Wall #-}
------------------------------------------------------------
-- |
-- Module       : Data.PurePriorityQueue
-- Copyright    : (c) 2009 Bradford Larsen
-- License      : BSD-style
-- Maintainer   : brad.larsen@gmail.com
--
-- A pure priority queue.
--
-- Because many function names clash with "Prelude" names, this module
-- is usually imported @qualified@, e.g.,
--
-- >  import qualified Data.PurePriorityQueue as PQ
--
-- This implementation is built on top of "Data.Map".
--
-- Estimates of worst-case time complexity are given.  The value /n/
-- is the number of elements in the queue.  The value /p/ is the
-- cardinality of the set of priorities of the elements in the queue.
-- /p/ is never greater than /n/.
------------------------------------------------------------

module Data.PurePriorityQueue
  (
    MinMaxQueue
  , empty
  , null
  , singleton
  , insert
  , deleteMin
  , deleteMax
  , minView
  , maxView
  , minPriority
  , maxPriority
  , foldWithPriority
  , splitByPriority
  , size
  , filter
  , filterWithPriority
  , toAscList
  ) where


import Data.PurePriorityQueue.Internal
import Prelude hiding (null, filter)
