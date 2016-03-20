{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Class.AdjacencyList
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  type families
--
----------------------------------------------------------------------------

module Data.Graph.Class.AdjacencyList
  ( AdjacencyListGraph(..)
  , defaultOutEdges
  , module Data.Graph.Class
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Error
import Control.Monad.Trans.Reader
import Data.Functor.Identity
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.Graph.Class

-- | 'defaultOutEdges': TODO
defaultOutEdges :: AdjacencyListGraph g => Vertex g -> g [(Vertex g, Vertex g)]
defaultOutEdges v = liftM (map ((,) v)) (adjacentVertices v)

-- | Minimal definition: 'source', 'target', and either 'adjacentVertices' with @'outEdges' = 'defaultOutEdges'@ or 'outEdges'
class Graph g => AdjacencyListGraph g where
  -- | 'source': Complexity /O(1)/
  source :: Edge g -> g (Vertex g)
  -- | 'target': Complexity: /O(1)/
  target :: Edge g -> g (Vertex g)
  -- | 'outEdges': Complexity /O(e)/ in the number of out edges
  outEdges :: Vertex g -> g [Edge g]

  -- | 'outDegree': /O(e)/
  outDegree :: Vertex g -> g Int
  outDegree v = liftM length (outEdges v)

  -- | 'adjacentVertices': TODO
  adjacentVertices :: Vertex g -> g [Vertex g]
  adjacentVertices = outEdges >=> mapM target

instance AdjacencyListGraph g => AdjacencyListGraph (Strict.StateT s g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance AdjacencyListGraph g => AdjacencyListGraph (Lazy.StateT s g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (AdjacencyListGraph g, Monoid m) => AdjacencyListGraph (Strict.WriterT m g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (AdjacencyListGraph g, Monoid m) => AdjacencyListGraph (Lazy.WriterT m g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (AdjacencyListGraph g, Monoid m) => AdjacencyListGraph (Strict.RWST r m s g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (AdjacencyListGraph g, Monoid m) => AdjacencyListGraph (Lazy.RWST r m s g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance AdjacencyListGraph g => AdjacencyListGraph (ReaderT e g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance (AdjacencyListGraph g, Error e) => AdjacencyListGraph (ErrorT e g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance AdjacencyListGraph g => AdjacencyListGraph (MaybeT g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance AdjacencyListGraph g => AdjacencyListGraph (IdentityT g) where
  adjacentVertices = lift . adjacentVertices
  source = lift . source
  target = lift . target
  outEdges = lift . outEdges
  outDegree = lift . outDegree

instance AdjacencyListGraph Identity where
  source = Identity
  target = Identity
  outEdges _ = Identity []
  outDegree _ = Identity 0
