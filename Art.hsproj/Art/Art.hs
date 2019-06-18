module Art where  

import ShapeGraphics
import Codec.Picture

art :: Picture
art = fracTree 15 100 10 -- replace with something else

fracTree :: Float -> Float -> Int -> Picture
fracTree width height n
  = fTree (Point  (400 - width / 2) 800) (Vector 0 (-height))
                  (Vector width 0) red n
  where
    
    toBlue :: Colour -> Colour
    toBlue (Colour r g b o) = 
      Colour (max 0 (r - 15)) g (min 255 (b + 15)) o
    angle = pi/8
    fTree :: Point -> Vector -> Vector -> Colour -> Int -> Picture
    fTree pos vec1 vec2 col n
      | n <= 1 = [Polygon [pos, movePoint vec1 pos, 
                                movePoint vec2 $ movePoint vec1 pos, 
                                movePoint vec2 pos]
                          col
                          Solid
                          SolidFill]
                          
      | otherwise = fTree pos vec1 vec2 col (n - 1) ++
                    fTree (movePoint vec1 pos) 
                          (scaleVector 0.8 $ rotateVector (0.5 * angle) vec1)
                          (scaleVector 0.8 $ rotateVector (0.5 * angle) vec2) 
                          (toBlue col) (n - 1) ++
                    fTree (movePoint vec1 pos) 
                          (scaleVector 0.8 $ rotateVector (-angle) vec1)
                          (scaleVector 0.8 $ rotateVector (-angle) vec2) 
                          (toBlue col) (n - 1) 

      
scaleVector :: Float -> Vector -> Vector
scaleVector fac (Vector x y)
  = Vector (fac * x) (fac * y)                           
 
rotateVector :: Float -> Vector -> Vector
rotateVector alpha (Vector vx vy)
  = Vector (cos alpha * vx - sin alpha * vy)
           (sin alpha * vx + cos alpha * vy)

movePoint :: Vector -> Point -> Point
movePoint (Vector xv yv) (Point xp yp)
  = Point (xv + xp) (yv + yp)
  

-- my Art


eye :: Picture
eye = [triangle, eyeball, eyeout]
  where 
    eyeball :: PictureObject
    eyeball = Circle (Point 400 400) 70 white Solid NoFill
    eyeout :: PictureObject
    eyeout = Ellipse (Point 400 400) 180 70 0 white Solid NoFill 
    triangle :: PictureObject
    triangle = Polygon points red Solid SolidFill
      where points = [Point 100 550, Point 700 550, Point 400 150] 




-- This is adapted from learning Haskell
rotateLine :: Float -> Line -> Line
rotateLine alpha (Line (Point x1 y1) (Point x2 y2))
  = Line (Point x1 y1) (Point (x' + x1) (y' + y1))
  where
      x0 = x2 - x1
      y0 = y2 - y1
      x' = x0 * cos alpha - y0 * sin alpha
      y' = x0 * sin alpha + y0 * cos alpha
      

spiralLines :: Float -> Int -> Line -> [Line]
spiralLines angle n line
  = spiralLines' n line
  where
    spiralLines' n (Line p1 p2)
      | n <= 0 = []
      | otherwise = (Line p1 p2) : spiralLines' (n-1) (rotateLine angle $ Line p1 p2)


lineToPicObj :: Line -> Colour -> PictureObject
lineToPicObj (Line p1 p2) colour = Path [p1, p2] colour Solid


spiralRays :: Picture
spiralRays = map ( `lineToPicObj` blue) lines
  where lines = spiralLines (pi / 100) 200 $ Line (Point 400 400) (Point 400 0)


-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "art.png" (drawPicture 3 art)
  



