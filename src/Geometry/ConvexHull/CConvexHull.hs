-- {-# LINE 1 "convexhull.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Geometry.ConvexHull.CConvexHull
  ( 
    cConvexHullToConvexHull
  , c_convexhull 
  )
  where
import           Control.Monad             ( (<$!>) )
import           Geometry.ConvexHull.Types ( ConvexHull(..)
                                           , Facet(..)
                                           , Ridge(..)
                                           , Vertex(..) )
import           Data.IntMap.Strict        ( IntMap, fromAscList )
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import           Data.List                 ( zip4 )
import qualified Data.HashMap.Strict.InsOrd as H
import           Data.Tuple.Extra          ( both )
import           Foreign                   ( Ptr
                                           , Storable(
                                               pokeByteOff
                                             , poke
                                             , peek
                                             , alignment
                                             , sizeOf
                                             , peekByteOff
                                             )
                                           , peekArray )
import           Foreign.C.Types           ( CInt, CDouble, CUInt(..) )
import           Foreign.C.String          ( CString )
import           Geometry.Qhull.Types      ( Family(Family, None), IndexPair(Pair) )



data CVertex = CVertex {
    __id :: CUInt
  , __point :: Ptr CDouble
}

instance Storable CVertex where
    sizeOf    __ = (16)
-- {-# LINE 42 "convexhull.hsc" #-}
    alignment __ = 8
-- {-# LINE 43 "convexhull.hsc" #-}
    peek ptr = do
      id'     <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
-- {-# LINE 45 "convexhull.hsc" #-}
      point'  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
-- {-# LINE 46 "convexhull.hsc" #-}
      return CVertex { __id = id'
                     , __point = point' }
    poke ptr (CVertex r1 r2)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
-- {-# LINE 51 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
-- {-# LINE 52 "convexhull.hsc" #-}

cVerticesToMap :: Int -> [CVertex] -> IO (IntMap [Double])
cVerticesToMap dim cvertices = do
  let ids = map (fromIntegral . __id) cvertices
  points <- mapM (\cv -> (<$!>) (map realToFrac) (peekArray dim (__point cv)))
                 cvertices
  return $ IM.fromList (zip ids points)

cVerticesToMap' :: Int -> [CVertex] -> IO (IntMap [Double])
cVerticesToMap' dim cvertices = do
  let ids = map (fromIntegral . __id) cvertices
  points <- mapM (\cv -> (<$!>) (map realToFrac) (peekArray dim (__point cv)))
                 cvertices
  return $ fromAscList (zip ids points)

data CVertex' = CVertex' {
    __id' :: CUInt
  , __point' :: Ptr CDouble
  , __neighfacets :: Ptr CUInt
  , __nneighfacets :: CUInt
  , __neighvertices :: Ptr CUInt
  , __nneighvertices :: CUInt
  , __neighridges :: Ptr CUInt
  , __nneighridges :: CUInt
}

instance Storable CVertex' where
    sizeOf    __ = (64)
-- {-# LINE 80 "convexhull.hsc" #-}
    alignment __ = 8
-- {-# LINE 81 "convexhull.hsc" #-}
    peek ptr = do
      id'              <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
-- {-# LINE 83 "convexhull.hsc" #-}
      point'           <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
-- {-# LINE 84 "convexhull.hsc" #-}
      neighfacets'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
-- {-# LINE 85 "convexhull.hsc" #-}
      nneighfacets'    <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
-- {-# LINE 86 "convexhull.hsc" #-}
      neighvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
-- {-# LINE 87 "convexhull.hsc" #-}
      nneighsvertices' <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
-- {-# LINE 88 "convexhull.hsc" #-}
      neighridges'     <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
-- {-# LINE 89 "convexhull.hsc" #-}
      nneighridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
-- {-# LINE 90 "convexhull.hsc" #-}
      return CVertex' { __id'            = id'
                      , __point'         = point'
                      , __neighfacets    = neighfacets'
                      , __nneighfacets   = nneighfacets'
                      , __neighvertices  = neighvertices'
                      , __nneighvertices = nneighsvertices'
                      , __neighridges    = neighridges'
                      , __nneighridges   = nneighridges'
                      }
    poke ptr (CVertex' r1 r2 r3 r4 r5 r6 r7 r8)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
-- {-# LINE 102 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
-- {-# LINE 103 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
-- {-# LINE 104 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
-- {-# LINE 105 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
-- {-# LINE 106 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr r6
-- {-# LINE 107 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr r7
-- {-# LINE 108 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr r8
-- {-# LINE 109 "convexhull.hsc" #-}

cVerticesToVertexMap :: Int -> [CVertex'] -> IO (IntMap Vertex)
cVerticesToVertexMap dim cvertices = do
  let ids             = map (fromIntegral . __id') cvertices
      nneighfacets    = map (fromIntegral . __nneighfacets) cvertices
      nneighsvertices = map (fromIntegral . __nneighvertices) cvertices
      nneighridges    = map (fromIntegral . __nneighridges) cvertices
  points <- mapM (\cv -> (<$!>) (map realToFrac) (peekArray dim (__point' cv)))
                 cvertices
  neighfacets <- mapM (\(i, cv) -> (<$!>) (map fromIntegral)
                                          (peekArray i (__neighfacets cv)))
                       (zip nneighfacets cvertices)
  neighvertices <- mapM (\(i, cv) ->
                          (<$!>) (map fromIntegral)
                                 (peekArray i (__neighvertices cv)))
                        (zip nneighsvertices cvertices)
  neighridges <- mapM (\(i, cv) ->
                       (<$!>) (map fromIntegral)
                              (peekArray i (__neighridges cv)))
                     (zip nneighridges cvertices)
  return $ IM.fromList
           (zip ids (map (\(pt, fneighs, vneighs, eneighs) ->
                          Vertex { _point         = pt
                                 , _neighfacets   = IS.fromAscList fneighs
                                 , _neighvertices = IS.fromAscList vneighs
                                 , _neighridges   = IS.fromAscList eneighs })
                          (zip4 points neighfacets neighvertices neighridges)))

data CRidge = CRidge {
    __rvertices :: Ptr CVertex
  , __ridgeOf1 :: CUInt
  , __ridgeOf2 :: CUInt
  , __ridgeSize :: CUInt
  , __ridgeid   :: CUInt
  , __redges :: Ptr (Ptr CUInt)
  , __nredges :: CUInt
}

instance Storable CRidge where
    sizeOf    __ = (40)
-- {-# LINE 149 "convexhull.hsc" #-}
    alignment __ = 8
-- {-# LINE 150 "convexhull.hsc" #-}
    peek ptr = do
      rvertices <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
-- {-# LINE 152 "convexhull.hsc" #-}
      ridgeOf1' <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
-- {-# LINE 153 "convexhull.hsc" #-}
      ridgeOf2' <- (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
-- {-# LINE 154 "convexhull.hsc" #-}
      ridgeSize <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
-- {-# LINE 155 "convexhull.hsc" #-}
      ridgeid   <- (\hsc_ptr -> peekByteOff hsc_ptr 20) ptr
-- {-# LINE 156 "convexhull.hsc" #-}
      edges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
-- {-# LINE 157 "convexhull.hsc" #-}
      nedges'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
-- {-# LINE 158 "convexhull.hsc" #-}
      return CRidge { __rvertices = rvertices
                    , __ridgeOf1  = ridgeOf1'
                    , __ridgeOf2  = ridgeOf2'
                    , __ridgeSize = ridgeSize
                    , __ridgeid   = ridgeid
                    , __redges    = edges'
                    , __nredges   = nedges' }
    poke ptr (CRidge r1 r2 r3 r4 r5 r6 r7)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
-- {-# LINE 168 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
-- {-# LINE 169 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr r3
-- {-# LINE 170 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r4
-- {-# LINE 171 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 20) ptr r5
-- {-# LINE 172 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r6
-- {-# LINE 173 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r7
-- {-# LINE 174 "convexhull.hsc" #-}

cRidgeToRidge :: Int -> CRidge -> IO (Int, Ridge)
cRidgeToRidge dim cridge = do
  let f1     = fromIntegral $ __ridgeOf1 cridge
      f2     = fromIntegral $ __ridgeOf2 cridge
      n      = fromIntegral $ __ridgeSize cridge
      rid    = fromIntegral $ __ridgeid cridge
      nedges = fromIntegral $ __nredges cridge
  vertices <- peekArray n (__rvertices cridge)
  rvertices <- cVerticesToMap' dim vertices
  edges' <- if dim > 3
            then (<$!>) (map (\x -> (fromIntegral (x!!0), fromIntegral (x!!1))))
                        ((=<<) (mapM (peekArray 2))
                               (peekArray nedges (__redges cridge)))
            else return []
  let edges = if dim > 3
              then H.fromList (zip (map (\(i,j) -> Pair i j) edges')
                                   (map (both ((IM.!) rvertices)) edges'))
              else H.empty
  return (rid, Ridge { _rvertices = rvertices
                     , _ridgeOf   = IS.fromAscList [f1,f2]
                     , _redges    = edges })

data CFace = CFace {
    __fvertices :: Ptr CVertex
  , __nvertices' :: CUInt
  , __ridges :: Ptr CUInt
  , __nridges' :: CUInt
  , __center :: Ptr CDouble
  , __normal :: Ptr CDouble
  , __offset :: CDouble
  , __orientation :: CInt
  , __area :: CDouble
  , __neighbors :: Ptr CUInt
  , __neighborsize :: CUInt
  , __family :: CInt
  , __edges :: Ptr (Ptr CUInt)
  , __nedges :: CUInt
}

instance Storable CFace where
    sizeOf    __ = (112)
-- {-# LINE 216 "convexhull.hsc" #-}
    alignment __ = 8
-- {-# LINE 217 "convexhull.hsc" #-}
    peek ptr = do
      fvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
-- {-# LINE 219 "convexhull.hsc" #-}
      nvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
-- {-# LINE 220 "convexhull.hsc" #-}
      ridges'      <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
-- {-# LINE 221 "convexhull.hsc" #-}
      nridges'     <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
-- {-# LINE 222 "convexhull.hsc" #-}
      center'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
-- {-# LINE 223 "convexhull.hsc" #-}
      normal'      <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
-- {-# LINE 224 "convexhull.hsc" #-}
      offset'      <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
-- {-# LINE 225 "convexhull.hsc" #-}
      orientation' <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
-- {-# LINE 226 "convexhull.hsc" #-}
      area'        <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
-- {-# LINE 227 "convexhull.hsc" #-}
      neighbors'   <- (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
-- {-# LINE 228 "convexhull.hsc" #-}
      neighsize    <- (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
-- {-# LINE 229 "convexhull.hsc" #-}
      family'      <- (\hsc_ptr -> peekByteOff hsc_ptr 92) ptr
-- {-# LINE 230 "convexhull.hsc" #-}
      edges'       <- (\hsc_ptr -> peekByteOff hsc_ptr 96) ptr
-- {-# LINE 231 "convexhull.hsc" #-}
      nedges'      <- (\hsc_ptr -> peekByteOff hsc_ptr 104) ptr
-- {-# LINE 232 "convexhull.hsc" #-}
      return CFace { __fvertices    = fvertices'
                   , __nvertices'   = nvertices'
                   , __ridges       = ridges'
                   , __nridges'     = nridges'
                   , __center       = center'
                   , __normal       = normal'
                   , __offset       = offset'
                   , __orientation  = orientation'
                   , __area         = area'
                   , __neighbors    = neighbors'
                   , __neighborsize = neighsize
                   , __family       = family'
                   , __edges        = edges'
                   , __nedges       = nedges'
                 }
    poke ptr (CFace r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)     ptr r1
-- {-# LINE 250 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)    ptr r2
-- {-# LINE 251 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)    ptr r3
-- {-# LINE 252 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)      ptr r4
-- {-# LINE 253 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)       ptr r5
-- {-# LINE 254 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)       ptr r6
-- {-# LINE 255 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)       ptr r7
-- {-# LINE 256 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 64)  ptr r8
-- {-# LINE 257 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 72)         ptr r9
-- {-# LINE 258 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 80)    ptr r10
-- {-# LINE 259 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 88) ptr r11
-- {-# LINE 260 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 92)       ptr r12
-- {-# LINE 261 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 96)        ptr r13
-- {-# LINE 262 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 104)       ptr r14
-- {-# LINE 263 "convexhull.hsc" #-}

cFaceToFacet :: Int -> CFace -> IO Facet
cFaceToFacet dim cface = do
  let area        = realToFrac (__area cface)
      neighsize   = fromIntegral (__neighborsize cface)
      offset      = realToFrac (__offset cface)
      orientation = fromIntegral (__orientation cface)
      family      = fromIntegral (__family cface)
      nridges     = fromIntegral (__nridges' cface)
      nvertices   = fromIntegral (__nvertices' cface)
      nedges      = fromIntegral (__nedges cface)
  center    <- (<$!>) (map realToFrac) (peekArray dim (__center cface))
  normal    <- (<$!>) (map realToFrac) (peekArray dim (__normal cface))
  vertices  <- (=<<) (cVerticesToMap dim)
                     (peekArray nvertices (__fvertices cface))
  ridges  <- (<$!>) (map fromIntegral)
                    (peekArray nridges (__ridges cface))
  neighbors <- (<$!>) (map fromIntegral)
                      (peekArray neighsize (__neighbors cface))
  edges' <- (<$!>) (map (\x -> (fromIntegral (x!!0), fromIntegral (x!!1))))
                      ((=<<) (mapM (peekArray 2))
                             (peekArray nedges (__edges cface)))
  let edges = H.fromList
              (zip (map (\(i,j) -> Pair i j) edges')
                   (map (both ((IM.!) vertices)) edges'))
  return Facet { _fvertices    = vertices
               , _fridges      = IS.fromAscList ridges
               , _centroid     = center
               , _normal'      = normal
               , _offset'      = offset
               , _orientation' = orientation
               , _area         = area
               , _neighbors    = IS.fromAscList neighbors
               , _family'      = if family == -1 then None else Family family
               , _fedges       = edges }

data CConvexHull = CConvexHull {
    __dim    :: CUInt
  , __allvertices :: Ptr CVertex'
  , __nvertices :: CUInt
  , __faces :: Ptr CFace
  , __nfaces :: CUInt
  , __allridges :: Ptr CRidge
  , __nridges :: CUInt
  , __alledges :: Ptr (Ptr CUInt)
  , __nalledges :: CUInt
  , __triangle :: CUInt
}

instance Storable CConvexHull where
    sizeOf    __ = (72)
-- {-# LINE 314 "convexhull.hsc" #-}
    alignment __ = 8
-- {-# LINE 315 "convexhull.hsc" #-}
    peek ptr = do
      dim'         <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
-- {-# LINE 317 "convexhull.hsc" #-}
      vertices'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
-- {-# LINE 318 "convexhull.hsc" #-}
      nvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
-- {-# LINE 319 "convexhull.hsc" #-}
      faces'       <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
-- {-# LINE 320 "convexhull.hsc" #-}
      nfaces'      <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
-- {-# LINE 321 "convexhull.hsc" #-}
      allridges'   <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
-- {-# LINE 322 "convexhull.hsc" #-}
      nridges'     <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
-- {-# LINE 323 "convexhull.hsc" #-}
      alledges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
-- {-# LINE 324 "convexhull.hsc" #-}
      nedges'      <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
-- {-# LINE 325 "convexhull.hsc" #-}
      triangle'    <- (\hsc_ptr -> peekByteOff hsc_ptr 68) ptr
-- {-# LINE 326 "convexhull.hsc" #-}
      return CConvexHull { __dim         = dim'
                         , __allvertices = vertices'
                         , __nvertices   = nvertices'
                         , __faces       = faces'
                         , __nfaces      = nfaces'
                         , __allridges   = allridges'
                         , __nridges     = nridges'
                         , __alledges    = alledges'
                         , __nalledges   = nedges'
                         , __triangle    = triangle'
                     }
    poke ptr (CConvexHull r1 r2 r3 r4 r5 r6 r7 r8 r9 r10)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
-- {-# LINE 340 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
-- {-# LINE 341 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
-- {-# LINE 342 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
-- {-# LINE 343 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
-- {-# LINE 344 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr r6
-- {-# LINE 345 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr r7
-- {-# LINE 346 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr r8
-- {-# LINE 347 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 64) ptr r9
-- {-# LINE 348 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 68) ptr r10
-- {-# LINE 349 "convexhull.hsc" #-}

foreign import ccall unsafe "convexHull" c_convexhull
  :: Ptr CDouble -- points
  -> CUInt -- dim
  -> CUInt -- npoints
  -> CUInt -- triangulate
  -> CUInt -- print to stdout
  -> CString -- summary file
  -> Ptr CUInt -- exitcode
  -> IO (Ptr CConvexHull)

cConvexHullToConvexHull :: CConvexHull -> IO ConvexHull
cConvexHullToConvexHull cconvexhull = do
  let dim       = fromIntegral (__dim cconvexhull)
      nvertices = fromIntegral (__nvertices cconvexhull)
      nfaces    = fromIntegral (__nfaces cconvexhull)
      nridges   = fromIntegral (__nridges cconvexhull)
      nedges    = fromIntegral (__nalledges cconvexhull)
      triangle  = fromIntegral (__triangle cconvexhull)
  vertices <- (=<<) (cVerticesToVertexMap dim)
                    (peekArray nvertices (__allvertices cconvexhull))
  faces <- (=<<) (mapM (cFaceToFacet dim))
                       (peekArray nfaces (__faces cconvexhull))
  allridges <- (=<<) (mapM (cRidgeToRidge dim))
                          (peekArray nridges (__allridges cconvexhull))
  alledges' <- (<$!>) (map (\x -> (fromIntegral (x!!0), fromIntegral (x!!1))))
                      ((=<<) (mapM (peekArray 2))
                             (peekArray nedges (__alledges cconvexhull)))
  let alledges = let points = IM.map _point vertices in
                  H.fromList
                  (zip (map (\(i,j) -> Pair i j) alledges')
                       (map (both ((IM.!) points)) alledges'))
  return ConvexHull {
                        _hvertices  = vertices
                      , _hfacets    = fromAscList (zip [0 .. nfaces-1] faces)
                      , _hridges    = fromAscList allridges
                      , _hedges     = alledges
                      , _simplicial = triangle == (1::Int)
                      , _dimension  = dim
                    }
