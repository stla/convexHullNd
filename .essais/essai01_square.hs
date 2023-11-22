import Geometry.ConvexHull
import Text.Show.Pretty

square :: [[Double]]
square = [
           [0, 0]
         , [1, 1]
         , [0, 1]
         , [1, 0]
         ]

hull :: IO ConvexHull
hull = convexHull square True True (Just "essai01.txt")
