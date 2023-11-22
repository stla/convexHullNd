# convexHullNd

<!-- badges: start -->
[![Stack-lts](https://github.com/stla/convexHullNd/actions/workflows/Stack-lts.yml/badge.svg)](https://github.com/stla/convexHullNd/actions/workflows/Stack-lts.yml)
[![Stack-lts-Mac](https://github.com/stla/convexHullNd/actions/workflows/Stack-lts-Mac.yml/badge.svg)](https://github.com/stla/convexHullNd/actions/workflows/Stack-lts-Mac.yml)
[![Stack-nightly](https://github.com/stla/convexHullNd/actions/workflows/Stack-nightly.yml/badge.svg)](https://github.com/stla/convexHullNd/actions/workflows/Stack-nightly.yml)
<!-- badges: end -->

*Convex hull in arbitrary dimension.*

___

The main function of this package is `convexHull`:

```haskell
convexHull :: [[Double]]     -- ^ vertices
           -> Bool           -- ^ whether to triangulate
           -> Bool           -- ^ whether to print output to stdout
           -> Maybe FilePath -- ^ write summary to a file
           -> IO ConvexHull
```

The first argument is the list of the Cartesian coordinates of the points for 
which the convex hull is wanted. The second argument indicates whether one 
wants a "triangulated" convex hull. In 3D this means all facets of the hull 
are triangles. In 4D this means they all are tetrahedra. The correct word for 
any dimension is "simplex". 

A `ConvexHull` object has the following fields:

- `_hvertices`: provides the vertices of the convex hull;

- `_hfacets`: provides the facets of the convex hull;

- `_hridges`: provides the ridges (facets of facets) of the convex hull; 

- `_hedges`: provides the edges of the convex hull.

```haskell
ConvexHull
  { _hvertices =
      fromList
        [ ( 0
          , Vertex
              { _point = [ 0.0 , 0.0 , 0.0 ]
              , _neighfacets = fromList [ 0 , 1 , 3 ]
              , _neighvertices = fromList [ 2 , 3 , 4 ]
              , _neighridges = fromList [ 1 , 2 , 4 ]
              }
          )
        , ...

  , _hfacets =
      fromList
        [ ( 0
          , Facet
              { _fvertices =
                  fromList
                    [ ( 0 , [ 0.0 , 0.0 , 0.0 ] )
                    , ( 1 , [ 0.0 , 1.0 , 1.0 ] )
                    , ( 2 , [ 0.0 , 0.0 , 1.0 ] )
                    , ( 3 , [ 0.0 , 1.0 , 0.0 ] )
                    ]
              , _fridges = fromList [ 0 , 1 , 2 , 3 ]
              , _centroid = [ 0.0 , 0.5 , 0.5 ]
              , _normal' = [ -1.0 , 0.0 , -0.0 ]
              , _offset' = -0.0
              , _orientation' = 1
              , _area = 1.0
              , _neighbors = fromList [ 1 , 2 , 3 , 5 ]
              , _family' = None
              , _fedges =
                  fromList
                    [ ( Pair 0 2 , ( [ 0.0 , 0.0 , 0.0 ] , [ 0.0 , 0.0 , 1.0 ] ) )
                    , ( Pair 0 3 , ( [ 0.0 , 0.0 , 0.0 ] , [ 0.0 , 1.0 , 0.0 ] ) )
                    , ( Pair 1 2 , ( [ 0.0 , 1.0 , 1.0 ] , [ 0.0 , 0.0 , 1.0 ] ) )
                    , ( Pair 1 3 , ( [ 0.0 , 1.0 , 1.0 ] , [ 0.0 , 1.0 , 0.0 ] ) )
                    ]
              }
          )
        , ...

  , _hridges =
      fromList
        [ ( 0
          , Ridge
              { _rvertices =
                  fromList
                    [ ( 1 , [ 0.0 , 1.0 , 1.0 ] ) , ( 2 , [ 0.0 , 0.0 , 1.0 ] ) ]
              , _ridgeOf = fromList [ 0 , 2 ]
              , _redges = fromList []
              }
          )
        , ...

  , _hedges =
      fromList
        [ ( Pair 0 2 , ( [ 0.0 , 0.0 , 0.0 ] , [ 0.0 , 0.0 , 1.0 ] ) )
        , ...
```

This is a big object. The function `hullSummary` returns a summary of it.

The `hullVolume` function returns the volume of the convex hull in any 
dimension (area in dimension 2, volume in dimension 3, hypervolume in higher 
dimension). 

Another useful function is `hullToSTL` for dimension 3. It writes a STL file 
of the mesh representing the convex hull. One can visualize it in e.g. MeshLab.