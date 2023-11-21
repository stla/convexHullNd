module Geometry.ConvexHull.ConvexHull
  where
import           Control.Monad                   ( unless, when )
import           Geometry.ConvexHull.CConvexHull
import           Geometry.ConvexHull.Types
import           Data.Function                   ( on )
import           Data.Graph                      ( flattenSCCs, stronglyConnComp )
import qualified Data.HashMap.Strict.InsOrd      as H
import           Data.IntMap.Strict              ( IntMap )
import qualified Data.IntMap.Strict              as IM
import qualified Data.IntSet                     as IS
import           Data.List
import           Data.List.Index                 ( imap )
import           Data.List.Unique                ( allUnique, count_ )
import           Data.Tuple.Extra                ( both )
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc           ( free, mallocBytes )
import           Foreign.Marshal.Array           ( pokeArray )
import           Foreign.Storable                ( peek, sizeOf )
import           Geometry.Qhull.Shared
import           Geometry.Qhull.Types
import           Text.Printf
-- import           Text.Regex

convexHull :: [[Double]]     -- ^ vertices
           -> Bool           -- ^ whether to triangulate
           -> Bool           -- ^ print output to stdout
           -> Maybe FilePath -- ^ write summary to a file
           -> IO ConvexHull
convexHull points triangulate stdout file = do
  let n   = length points
      dim = length (head points)
  when (dim < 2) $
    error "dimension must be at least 2"
  unless (all (== dim) (map length (tail points))) $
    error "the points must have the same dimension"
  when (n <= dim) $
    error "insufficient number of points"
  unless (allUnique points) $
    error "some points are duplicated"
  pointsPtr <- mallocBytes (n * dim * sizeOf (undefined :: CDouble))
  pokeArray pointsPtr (concatMap (map realToFrac) points)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  summaryFile <- maybe (newCString []) newCString file
  resultPtr <- c_convexhull pointsPtr (fromIntegral dim) (fromIntegral n)
               (fromIntegral $ fromEnum triangulate)
               (fromIntegral $ fromEnum stdout) summaryFile exitcodePtr
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free pointsPtr
  if exitcode /= 0
    then do
      free resultPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      result <- (>>=) (peek resultPtr) cConvexHullToConvexHull
      free resultPtr
      return result


-- | convex hull summary
hullSummary :: ConvexHull -> String
hullSummary hull =
  "Convex hull:\n" ++
  printf "%d vertices\n" (IM.size vertices) ++
  printf "%d facets (%s)\n" nfacets families ++
  printf "%d ridges\n" nridges ++
  printf "%d edges\n" nedges ++
  (if dim > 2
    then printf "number of vertices per facet: %s\n" (show counts_vertices) ++
         (if dim > 3
           then printf "number of edges per facet: %s\n" (show counts_edges) ++
                printf "number of ridges per facet: %s\n" (show counts_ridges)
           else "")
    else "")
  where
    vertices = _vertices hull
    nedges = nEdges hull
    nridges = IM.size (_hridges hull)
    facets = _hfacets hull
    facets' = IM.elems facets
    nfacets = IM.size facets
    (nf1,nf2) = both length $
                partition (None ==) (nubBy sameFamily (map _family facets'))
    families = show nf1 ++ " single, " ++
               show nf2 ++ if nf2 > 1 then " families" else " family"
    dim = length $ head (IM.elems vertices)
    counts_vertices = count_ (map nVertices facets')
    counts_edges = count_ (map nEdges facets')
    counts_ridges = count_ (map (IS.size . _fridges) facets')

-- | facets ids an edge belongs to
edgeOf :: ConvexHull -> (Index, Index) -> [Int]
edgeOf hull (v1,v2) = IM.keys $ IM.filter (elem (Pair v1 v2)) facetsEdges
  where
    facetsEdges = IM.map edgesIds (_hfacets hull)

-- | ridges of a facet
facetRidges :: ConvexHull -> Facet -> IntMap Ridge
facetRidges hull facet = IM.restrictKeys (_hridges hull) (_fridges facet)

-- | vertices ids of all facets
facetsVerticesIds :: ConvexHull -> [[Index]]
facetsVerticesIds hull = map verticesIds (IM.elems $ _hfacets hull)

-- | vertices ids of all ridges
ridgesVerticesIds :: ConvexHull -> [[Index]]
ridgesVerticesIds hull = map verticesIds (IM.elems (_hridges hull))

-- | group facets of the same family
groupedFacets :: ConvexHull -> [(Family, [IndexMap [Double]], [EdgeMap])]
groupedFacets hull =
  zip3 (map head families) verticesGroups edgesGroups
  where
    facets         = IM.elems (_hfacets hull)
    facetsGroups   = groupBy (sameFamily `on` _family) facets
    edgesGroups    = map (map _fedges) facetsGroups
    verticesGroups = map (map _fvertices) facetsGroups
    families       = map (map _family) facetsGroups

-- | group facets of the same family and merge vertices and edges
groupedFacets' :: ConvexHull -> [(Family, IndexMap [Double], EdgeMap)]
groupedFacets' hull =
  map (\(f,v,e) -> (f, foldr IM.union IM.empty v, foldr delta H.empty e))
      (groupedFacets hull)
  -- zip3 (map head families) (map (foldr IM.union IM.empty) verticesGroups)
  --      (map (foldr delta H.empty) edgesGroups)
  where
    -- facets         = IM.elems (_hfacets hull)
    -- facetsGroups   = groupBy (sameFamily `on` _family) facets
    -- edgesGroups    = map (map _fedges) facetsGroups
    -- verticesGroups = map (map _fvertices) facetsGroups
    -- families       = map (map _family) facetsGroups
    delta :: EdgeMap -> EdgeMap -> EdgeMap
    delta e1 e2 = H.difference (H.union e1 e2) (H.intersection e1 e2)

-- data Vertex3 = Vertex3 Double Double Double
--   deriving Show
--
-- toVertex3 :: [Double] -> Vertex3
-- toVertex3 xs = Vertex3 (xs!!0) (xs!!1) (xs!!2)

-- | for 3D only, orders the vertices of the facet (i.e. provides a polygon) ;
-- also returns a Boolean indicating the orientation of the vertices
facetToPolygon :: Facet -> ([(Index, [Double])], Bool)
facetToPolygon facet = (polygon, dotProduct > 0)
  where
  vs = IM.toList $ _vertices facet
  x = imap (\i v -> (v, i, findIndices (connectedVertices v) vs)) vs
    where
    connectedVertices :: (Index, [Double]) -> (Index, [Double]) -> Bool
    connectedVertices (i,_) (j,_) = Pair i j `H.member` _edges facet
  polygon = flattenSCCs (stronglyConnComp x)
  vertices = map snd polygon
  v1 = vertices!!0
  v2 = vertices!!1
  v3 = vertices!!2
  normal = crossProd (zipWith subtract v1 v2) (zipWith subtract v1 v3)
    where
    crossProd u v = [ u!!1 * v!!2 - u!!2 * v!!1
                    , u!!2 * v!!0 - u!!0 * v!!2
                    , u!!0 * v!!1 - u!!1 * v!!0 ]
  dotProduct = sum $ zipWith (*) normal (_normal facet)

-- | for 3D only, orders the vertices of the facet (i.e. provides a polygon)
-- in anticlockwise orientation
facetToPolygon' :: Facet -> [(Index, [Double])]
facetToPolygon' facet = if test then polygon else reverse polygon
  where
  (polygon, test) = facetToPolygon facet

-- -- | like `facetToPolygon`, but returns the vertices indices
-- facetToPolygon' :: Facet -> [Index]
-- facetToPolygon' facet = map fst $ flattenSCCs (stronglyConnComp x)
--   where
--     vs = IM.toList $ _vertices facet
--     x = imap (\i v -> (v, i, findIndices (connectedVertices v) vs)) vs
--     connectedVertices :: (Index, [Double]) -> (Index, [Double]) -> Bool
--     connectedVertices (i,_) (j,_) = Pair i j `H.member` _edges facet

-- | for 4D only, orders the vertices of a ridge (i.e. provides a polygon)
ridgeToPolygon :: Ridge -> [(Index, [Double])]
ridgeToPolygon ridge = flattenSCCs (stronglyConnComp x)
  where
  vs = IM.toList $ _vertices ridge
  x = imap (\i v -> (v, i, findIndices (connectedVertices v) vs)) vs
    where
    connectedVertices :: (Index, [Double]) -> (Index, [Double]) -> Bool
    connectedVertices (i,_) (j,_) = Pair i j `H.member` _edges ridge

-- -- | for 3D only, convert the convex hull to STL format
-- hullToSTL :: ConvexHull -> FilePath -> IO ()
-- hullToSTL chull filename = do
--   let facets = IM.elems (_hfacets chull)
--       normals = map _normal facets
--       facetNormals = map (\n -> "facet normal  " ++ stringify " " n ++ "\nouter loop\n")
--                      normals
--       polygons = map (\f -> "vertex  " ++
--                         (stringify "\nvertex  " . map snd . facetToPolygon') f) facets
--       vertices = map (++ "\nendloop\nendfacet\n") polygons
--       vertices' = map (\v -> subRegex (mkRegex "\\]") (subRegex (mkRegex "\\[") v "") "") vertices
--       out = subRegex (mkRegex ",") (concat [x ++ y | x <- facetNormals, y <- vertices']) " "
--   writeFile filename ("solid " ++ filename ++ " produced by QHULL\n" ++ out)
--   where
--     stringify :: Show a => String -> [a] -> String
--     stringify sep = intercalate sep . map show
