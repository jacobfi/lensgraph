{-# LANGUAGE Rank2Types #-}

module Graphs (Graph, v, adj) where

import Prelude
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set

data Graph t = Graph {
	_adj :: Map.Map t (Set.Set t)
} deriving Show

-- Lens for manipulating the vertex set.
v :: Ord t => Lens' (Graph t) (Set.Set t)
v f (Graph em) = fmap (\vs -> Graph $ Set.foldr (\v' em' -> case em ^. at v' of
		Just es -> at v' .~ Just es $ em'
		Nothing -> at v' .~ Just Set.empty $ em'
	) Map.empty $ vs) (f $ Map.keysSet em)
----

-- Lens for manipulating adjacency.
adj :: Ord t => t -> t -> Lens' (Graph t) Bool
adj from to f (Graph em) = fmap (\b -> case b of
		True -> Graph $ at from . _Just %~ (Set.insert to) $ em
		False -> Graph $ at from . _Just %~ (Set.delete to) $ em
	) (f (em ^. at from . _Just ^. contains to))
----

-- Depth-first-search
-- TODO: Make better.
dfs :: Ord t => Graph t -> t -> [t]
dfs g n = Set.foldr (\e vl -> vl ++ dfs g e) [n] (_adj g ^. at n . _Just)
----

-- Example use
g = Graph (Map.fromList [(3, Set.fromList [4]), (1, Set.fromList [2, 3])])
a = g ^. v -- View the vertex set v of the graph.
b = v .~ Set.fromList [42] $ g -- Set v to the vertex set {42}.
c = v %~ (Set.insert 2) $ g -- Insert the vertex 2 into the graph.
d = v %~ (Set.union $ Set.fromList [2, 4, 5]) $ g -- Insert the vertices 2, 4 and 5 into the graph.
e = g ^. adj 1 3 -- Is vertex 1 adjacent to 3?
f = adj 1 4 .~ True $ g -- Make it so that vertex 1 is adjacent to 4.
h = adj 3 4 .~ False $ f -- Remove the edge from vertex 3 to 4 in f.
