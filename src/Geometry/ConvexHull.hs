{-|
Module      : Geometry.ConvexHull
Description : Convex hull in arbitrary dimension.
Copyright   : (c) Stéphane Laurent, 2023
License     : GPL-3
Maintainer  : laurent_step@outlook.fr

See README for examples.
-}
module Geometry.ConvexHull (module X) where

import Geometry.ConvexHull.ConvexHull as X
  ( convexHull,
    edgeOf,
    facetRidges,
    facetToPolygon,
    facetToPolygon',
    facetsVerticesIds,
    groupedFacets,
    groupedFacets',
    hullSummary,
    hullToSTL,
    hullVolume,
    ridgeToPolygon,
    ridgesVerticesIds
  )
import Geometry.ConvexHull.Types as X
  ( ConvexHull (..),
    Facet (..),
    Ridge (..),
    Vertex (..)
  )
import Geometry.Qhull.Shared as X
  ( edgesCoordinates,
    edgesIds,
    edgesIds',
    isEdge,
    nEdges,
    nVertices,
    toPoints,
    toPoints',
    verticesCoordinates,
    verticesIds
  )
import Geometry.Qhull.Types as X
  ( EdgeMap,
    Family (..),
    HasCenter (..),
    HasEdges (..),
    HasFamily (..),
    HasNormal (..),
    HasVertices (..),
    HasVolume (..),
    Index,
    IndexMap,
    IndexPair (..),
    IndexSet
  )
