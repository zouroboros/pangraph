module Pangraph.Internal.Pangraph where

import qualified Data.Set as S

type Directed a b e = (a, b, e)
type Undirected a b e = (a, b, e)

data MixedEdge a b e = D (Directed a b e) | U (Undirected a b e)

-- | Base type for graphs, v is the vertex type,
-- uc the container for undirected edges,
-- dc the container for directed edges (arrows).
type Graph v ec = (S.Set v, ec)

-- | Type for an undirected graph
-- v is the type of the vertices
-- e the type of the edges.
type UndirectedGraph v e = Graph v (S.Set (Undirected v v e))

-- | Type for an directed graph.
type DirectedGraph v e = Graph v (S.Set (Directed v v e))

-- | Type for a mixed graph. A mixed graph is a grap containing directed as well
-- as undirected edges.
type MixedGraph v e = Graph v (S.Set (MixedEdge v v e))

-- | Multigraph type. A multigraph is a graph that contains multiple undirected
-- edges between two vertices.
type MultiGraph v e = Graph v [Undirected v v e]

-- | A quiver is graph that contains multiple directed edges between two vertices.
type Quiver v e = Graph v [Directed v v e]

-- | Mixed quiver: a graph that can contain multiple, directed or undirected,
-- edges between two graphs.
type MixedQuiver v e = Graph v [MixedEdge v v e]

-- | HyperQuiver is a graph where nodes can contain multiple nodes and edges can
-- have multiple endpoints.
type HyperQuiver v e = MixedQuiver (S.Set v) e

-- | All the vertices in the graph
vertices :: Graph v ec -> S.Set v
vertices (v, _) = v

edges :: Graph v ec -> ec
edges (_, e) = e
