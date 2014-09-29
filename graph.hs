{-# LANGUAGE Rank2Types #-}

module Graphs (Graph, v, adj) where

import Prelude
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

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

Graph g' = g
keys = Set.toList $ Map.keysSet g'
es = Set.fromList $ foldl (\a vs -> vs ++ a) []
                               (fmap (\v -> case Map.lookup v g' of
                                                Just xs -> Set.toList xs
                                                _ -> []) $ keys)

-- Topological sort 
topoSort g l
    | Set.size va == 0 = Just $ List.reverse l
    | hasCycle = Nothing
    | otherwise = work
    where va = g ^. v
          
          Graph g' = g

          keys = Set.toList $ Map.keysSet g'

          incEdges = Set.fromList $ foldl (\a vs -> vs ++ a) []
                               (fmap (\v -> case Map.lookup v g' of
                                                Just xs -> Set.toList xs
                                                _ -> []) $ keys)

          s = Set.difference va incEdges

          hasCycle = case Set.size s of
                         0 -> True
                         _ -> False

          work = let x:_ = Set.toList s in
                 let g'' = v .~ Set.delete x va $ g in
                     topoSort g'' (x:l)

-- Example use
g = Graph (Map.fromList [(3, Set.fromList [4]), (1, Set.fromList [2, 3])])

a = g ^. v -- View the vertex set v of the graph.
b = v .~ Set.fromList [42] $ g -- Set v to the vertex set {42}.
c = v %~ (Set.insert 2) $ g -- Insert the vertex 2 into the graph.
d = v %~ (Set.union $ Set.fromList [2, 4, 5]) $ g -- Insert the vertices 2, 4 and 5 into the graph.
e = g ^. adj 1 3 -- Is vertex 1 adjacent to 3?
f = adj 1 4 .~ True $ g -- Make it so that vertex 1 is adjacent to 4.
h = adj 3 4 .~ False $ f -- Remove the edge from vertex 3 to 4 in f.

tsex1 = topoSort d []
tsex2 = topoSort (adj 5 2 .~ True $ d) []
tsex3 = topoSort (adj 2 1 .~ True $ d) []