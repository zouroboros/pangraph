module Pangraph.Internal.Pangraph where

import qualified Data.Set as S
import qualified Data.Foldable as F

data None a = None

instance Foldable None where
  foldMap _ = mempty
  foldr _ z _ = z

type Directed a b e = (a, b, e)
type Undirected a b e = (a, b, e)

-- | Base type for graphs, v is the vertex type,
-- uc the container for undirected edges,
-- dc the container for directed edges (arrows).
type Graph v uc dc = (S.Set v, uc, dc)

-- | Type for an undirected graph
-- v is the type of the vertices
-- e the type of the edges.
type UndirectedGraph v e = Graph v (S.Set (Undirected v v e)) (None (Directed v v e))

-- | Type for an directed graph.
type DirectedGraph v e = Graph v (None (Undirected v v e)) (S.Set (Directed v v e))

-- | Type for a mixed graph. A mixed graph is a grap containing directed as well
-- as undirected edges.
type MixedGraph v e = Graph v (S.Set (Undirected v v e)) (S.Set (Directed v v e))

-- | Multigraph type. A multigraph is a graph that contains multiple undirected
-- edges between two vertices.
type MultiGraph v e = Graph v [Undirected v v e] (None (Directed v v e))

-- | A quiver is graph that contains multiple directed edges between two vertices.
type Quiver v e = Graph v (None (Undirected v v e)) [Directed v v e]

-- | Mixed quiver: a graph that can contain multiple, directed or undirected,
-- edges between two graphs.
type MixedQuiver v e = Graph v [Undirected v v e] [Directed v v e]

-- | HyperQuiver is a graph where nodes can contain multiple nodes and edges can
-- have multiple endpoints.
type HyperQuiver v e = MixedQuiver (S.Set v) e

-- | All the vertices in the graph
vertices :: Graph v uc dc -> S.Set v
vertices (v, _, _) = v

-- | All undirected vertices in a graph. The resulting map assigns to each vertex
-- a container with of the connected vertices.
undirected :: Graph v uc dc -> uc
undirected (_, uc, _) = uc

-- | All directed vertices.
directed :: Graph v uc dc -> dc
directed (_, _, dc) = dc

-- | All edges from the graph.
allEdges :: (Foldable uc, Foldable dc) =>
  Graph v (uc (Undirected v v e)) (dc (Directed v v e)) -> [(v, v, e, Bool)]
allEdges graph = let
  undirected' = map (\(a, b, e) -> (a, b, e, False)) $ F.toList $ undirected graph
  directed' = map (\(a, b, e) -> (a, b, e, True)) $ F.toList $ directed graph
  in directed' ++ undirected'
