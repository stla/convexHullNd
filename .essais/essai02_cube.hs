import Geometry.ConvexHull
import Text.Show.Pretty

cube :: [[Double]]
cube = [
         [0, 0, 0]
       , [0, 1, 1]
       , [0, 0, 1]
       , [0, 1, 0]
       , [1, 0, 0]
       , [1, 1, 1]
       , [1, 0, 1]
       , [1, 1, 0]
       ]

hull :: IO ConvexHull
hull = convexHull cube True True Nothing
