{-# LANGUAGE CPP, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm.DepthFirstSearch
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
-- Depth-first search
----------------------------------------------------------------------------

module Data.Graph.Algorithm.DepthFirstSearch
  ( dfs
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Foldable
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Data.Graph.Algorithm
import Data.Graph.Class
import Data.Graph.Class.AdjacencyList
import Data.Graph.PropertyMap
import Data.Graph.Internal.Color

getS :: Monad g => k -> StateT (PropertyMap g k v) g v
getS k = do
  m <- get
  lift (getP m k)

putS :: Monad g => k -> v -> StateT (PropertyMap g k v) g ()
putS k v = do
  m <- get
  m' <- lift $ putP m k v
  put m'

-- TODO: CPS transform?
-- | 'dfs': Given a 'GraphSearch' visitor and a starting 'Vertex', returns an
--   'AdjacencyListGraph' constructed to perform a depth-first search.
dfs :: (AdjacencyListGraph g, Monoid m) => GraphSearch g m -> Vertex g -> g m
dfs vis v0 = do
  m <- vertexMap White
  evalStateT (go v0) m where
  go v = do
    putS v Grey
    lhs <- lift $ enterVertex vis v
    adjs <- lift $ outEdges v
    let revAdjs = reverse adjs
    result <- foldrM
      (\e m -> do
        v' <- target e
        color <- getS v'
        liftM (mappend m) $ case color of
          White -> (liftM2 mappend) (lift $ enterEdge vis e) (go v')
          Grey  -> lift $ grayTarget vis e
          Black -> lift $ blackTarget vis e
      )
      mempty
      revAdjs
    putS v Black
    rhs <- lift $ exitVertex vis v
    return $ lhs `mappend` result `mappend` rhs
