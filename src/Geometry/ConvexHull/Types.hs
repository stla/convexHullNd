{-# LANGUAGE InstanceSigs #-}
module Geometry.ConvexHull.Types
  where
import           Data.IntMap.Strict   ( IntMap )
import qualified Data.IntMap.Strict   as IM
import           Data.IntSet          ( IntSet )
import           Geometry.Qhull.Types ( HasCenter(..)
                                      , HasVolume(..)
                                      , HasEdges(..)
                                      , HasVertices(..)
                                      , HasNormal(..)
                                      , HasFamily(..)
                                      , Family
                                      , EdgeMap
                                      , IndexSet
                                      , IndexMap )

data Vertex = Vertex {
    _point         :: [Double]
  , _neighfacets   :: IntSet
  , _neighvertices :: IndexSet
  , _neighridges   :: IntSet
} deriving Show

data Ridge = Ridge {
    _rvertices :: IndexMap [Double]
  , _ridgeOf   :: IntSet
  , _redges    :: EdgeMap
} deriving Show

instance HasVertices Ridge where
  _vertices :: Ridge -> IndexMap [Double]
  _vertices = _rvertices

instance HasEdges Ridge where
  _edges :: Ridge -> EdgeMap
  _edges = _redges

data Facet = Facet {
    _fvertices    :: IndexMap [Double]
  , _fridges      :: IntSet
  , _centroid     :: [Double]
  , _normal'      :: [Double]
  , _offset'      :: Double
  , _orientation' :: Int
  , _area         :: Double
  , _neighbors    :: IntSet
  , _family'      :: Family
  , _fedges       :: EdgeMap
} deriving Show

instance HasCenter Facet where
  _center :: Facet -> [Double]
  _center = _centroid

instance HasEdges Facet where
  _edges :: Facet -> EdgeMap
  _edges = _fedges

instance HasVertices Facet where
  _vertices :: Facet -> IndexMap [Double]
  _vertices = _fvertices

instance HasNormal Facet where
  _normal :: Facet -> [Double]
  _normal = _normal'
  _offset :: Facet -> Double
  _offset = _offset'

instance HasVolume Facet where
  _volume :: Facet -> Double
  _volume = _area

instance HasFamily Facet where
  _family :: Facet -> Family
  _family = _family'

data ConvexHull = ConvexHull {
    _hvertices  :: IndexMap Vertex
  , _hfacets    :: IntMap Facet
  , _hridges    :: IntMap Ridge
  , _hedges     :: EdgeMap
  , _simplicial :: Bool
  , _dimension  :: Int
} deriving Show

instance HasEdges ConvexHull where
  _edges :: ConvexHull -> EdgeMap
  _edges = _hedges

instance HasVertices ConvexHull where
  _vertices :: ConvexHull -> IndexMap [Double]
  _vertices hull = IM.map _point (_hvertices hull)
