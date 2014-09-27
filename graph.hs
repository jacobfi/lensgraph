{-# LANGUAGE Rank2Types #-}

module Graphs (Graph, v, adj) where

import Prelude
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set

data Graph t = Graph {
	_adj :: Map.Map t (Set.Set t)
} deriving Show

v :: Ord t => Lens' (Graph t) (Set.Set t)
v f (Graph em) = fmap (\vs -> Graph $ Set.foldr (\v' em' -> case em ^. at v' of
		Just es -> at v' .~ Just es $ em'
		Nothing -> at v' .~ Just Set.empty $ em'
	) Map.empty $ vs) (f $ Map.keysSet em)
----

adj :: Ord t => t -> t -> Lens' (Graph t) Bool
adj from to f (Graph em) = fmap (\b -> case b of
		True -> Graph $ at from . _Just %~ (Set.insert to) $ em
		False -> Graph $ at from . _Just %~ (Set.delete to) $ em
	) (f (em ^. at from . _Just ^. contains to))
----

-- Example use
g = Graph (Map.fromList [(3, Set.fromList [4]), (1, Set.fromList [2, 3])])
a = g ^. v
b = v .~ Set.fromList [42] $ g
c = v %~ (Set.insert 2) $ g
d = v %~ (Set.union $ Set.fromList [2, 4, 5]) $ g
