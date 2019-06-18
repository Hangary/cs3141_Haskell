module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import ShapeGraphics

-- Part 1
-- picture of a house
housePic :: Picture
housePic = [door, house]
  where
    house :: PictureObject
    house = Path (cosToPOs houseCOs) green Solid
    door :: PictureObject
    door  = Path (cosToPOs doorCOs) red Solid

-- these are the coordinates - convert them to a list of Point
houseCOs :: [(Float, Float)]
houseCOs = [(300, 750), (300, 450), (270, 450), (500, 200),
         (730, 450), (700, 450), (700, 750)]

doorCOs :: [(Float, Float)]
doorCOs = [(420, 750), (420, 550), (580, 550), (580, 750)]

chimneyHouseCOs :: [(Float, Float)]
chimneyHouseCOs = [(300, 750), (300, 450), (270, 450), (500, 200),
              (615, 325), (615, 250), (650, 250), (650, 363),
              (730, 450), (700, 450), (700, 750)]

smokeCOs :: [(Float, Float)]
smokeCOs = [(635, 240), (625, 230), (635, 220), (625, 210)]

grey :: Colour
grey = Colour 255 255 255 128

-- convert COs to Points
cosToPOs :: [(Float, Float)] -> [Point]
cosToPOs [] = []
cosToPOs ( (x,y) :xs ) = Point x y : cosToPOs xs

smoke :: PictureObject
smoke = Path (cosToPOs smokeCOs) grey Solid

chimneyHouse :: Picture
chimneyHouse = [door, chimneyhouse, smoke]
  where
    chimneyhouse :: PictureObject
    chimneyhouse = Path (cosToPOs chimneyHouseCOs) green Solid
    door :: PictureObject
    door  = Path (cosToPOs doorCOs) red Solid



-- Part 2
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)

movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path points colour lineStyle) 
  = Path movedPoints colour lineStyle
    where movedPoints = map (`movePoint` vec) points
movePictureObject vec (Circle center radius colour lineStyle fillStyle) 
  = Circle (movePoint center vec) radius colour lineStyle fillStyle
movePictureObject vec (Ellipse center width height rotation colour lineStyle fillStyle) 
  = Ellipse (movePoint center vec) width height rotation colour lineStyle fillStyle
movePictureObject vec (Polygon points colour lineStyle fillStyle) 
  = Polygon movedPoints colour lineStyle fillStyle
    where movedPoints = map (`movePoint` vec) points 


-- Part 3


-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n = map (radiusToCircle col) $ enumFromThenTo (400/n) (2 * 400/n) 400

radiusToCircle :: Colour -> Float -> PictureObject
radiusToCircle col radius = Circle (Point 400 400) radius col Solid SolidFill 

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)
