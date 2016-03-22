module Data.Graph.Internal.Color where

-- | 'Color': Used internally for keeping track of edge states.
data Color = White | Grey | Black deriving (Eq,Ord,Show,Read)
