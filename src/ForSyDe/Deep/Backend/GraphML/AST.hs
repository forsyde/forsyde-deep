-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Deep.Backend.GraphML.AST
-- Copyright   :  (c) ES Group, KTH/ICT/ES 2007-2013
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  forsyde-dev@ict.kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- AST covering the GraphML subset we are interested in for this backend
-----------------------------------------------------------------------------

--FIXME: the design of this module is ugly

module ForSyDe.Deep.Backend.GraphML.AST where

import ForSyDe.Deep.Netlist

type GraphMLGraphId = String
type GraphMLPortId = String
type GraphMLNodeId = String

-- | Main AST type, a graph
data GraphMLGraph = GraphMLGraph 
        GraphMLGraphId -- Graph id
        [GraphMLNode] -- Nodes 
        [GraphMLEdge] -- Edges


-- | Edge
data GraphMLEdge = GraphMLEdge 
  GraphMLNode    -- Origin node
  GraphMLPortId  -- Origin port id
  GraphMLNode    -- Target node
  GraphMLPortId  -- Target port id

-- | Node
data GraphMLNode = 
 ProcNode (NlNode GraphMLPortId) -- Netlist node indicating the input ports
                 [GraphMLPortId]  -- Output ports 
 -- FIXME: This representation is ugly
                                  |
 OutNode GraphMLNodeId GraphMLPortId  -- the special output node      
