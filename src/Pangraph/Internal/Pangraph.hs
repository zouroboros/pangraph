module Pangraph.Internal.Pangraph where

import qualified Data.Set as S
import qualified Data.Foldable as F
import qualified Data.Map as M

type Arrow a b e = (a, b, e)

-- | Base type for graphs, v is the vertex type,
-- uc the container for undirected edges,
-- ac the container for directed edges (arrows).
type Graph v uc ac = (S.Set (v, uc), ac)

-- | Type for an undirected graph
-- v is the type of the vertices
-- e the type of the edges.
type UndirectedGraph v e = Graph v (S.Set (v, e)) ()

-- | Type for an directed graph.
type DirectedGraph v e = Graph v () (S.Set (Arrow v v e))

-- | Type for a mixed graph. A mixed graph is a grap containing directed as well
-- as undirected edges.
type MixedGraph v e = Graph v (S.Set (v, e)) (S.Set (Arrow v v e))

-- | Multigraph type. A multigraph is a graph that contains multiple undirected
-- edges between two vertices.
type MultiGraph v e = Graph v [(v, e)] ()

-- | A quiver is graph that contains multiple directed edges between two vertices.
type Quiver v e = Graph v () [Arrow v v e]

-- | Mixed quiver: a graph that can contain multiple, directed or undirected,
-- edges between two graphs.
type MixedQuiver v e = Graph v [(v, e)] [Arrow v v e]

-- | All the vertices in the graph
vertices :: Ord v => Graph v ec ac -> S.Set v
vertices = (S.map fst) . fst

-- | All undirected vertices in a graph. The resulting map assigns to each vertex
-- a container with of the connected vertices.
undirected :: Eq v => Graph v ec ac -> M.Map v ec
undirected = M.fromAscList . S.toAscList . fst

-- | All directed vertices.
directed :: Graph v ec ac -> ac
directed = snd

-- | All edges from the graph. Undirected edges will be converted to directed
-- edges
allEdges :: (Foldable ec, Ord v, Ord e, Ord (ec (v, e)), Foldable ac) =>
  Graph v (ec (v, e)) (ac (v, v, e)) -> [(v, v, e)]
allEdges graph = let
  undirected' = S.fromList $ M.toList $ undirected graph
  edges2Arrows v1 edges = map (\(v2, e) -> (v1, v2, e)) (F.toList edges)
  undirected'' = F.toList (S.map (uncurry edges2Arrows) undirected')
  directed' = F.toList $ directed graph
  in directed' ++ concat undirected''
