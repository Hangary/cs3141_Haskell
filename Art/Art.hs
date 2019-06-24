module Art where  

import ShapeGraphics
import Codec.Picture

art :: Picture
art = spirals ++ eye
  where
    spirals :: [PictureObject]
    spirals = spiralLines 1.1 (pi/10 + 1) magenta $ Line (Point 395 395) (Point 405 405)
    eye :: [PictureObject]
    eye = [eyeball, eyeout]
      where 
        eyeball :: PictureObject
        eyeball = Circle (Point 400 400) 30 white Dashed NoFill
        eyeout :: PictureObject
        eyeout = Ellipse (Point 400 400) 90 35 0 white Solid NoFill 


-- we can generate different graphs by changing the factor, angle, colour and startLine
-- factor: decide how many lines for each sprials, should be above 1
-- angle: the angle of spirals
-- colour: the colour of spirals
-- startLine: the start line of spirals
spiralLines :: Float -> Float -> Colour -> Line -> [PictureObject]
spiralLines factor angle colour startLine
      | factor <= 1 = []
      | otherwise = (Path (spiral angle factor 100 startLine) colour Solid) : (spiralLines (factor - 0.02) angle (fadeColour colour) startLine)


-- this will generate a spiral
spiral :: Float -> Float -> Int -> Line -> [Point]
spiral angle scaleFactor n line
  = spiral' n line
  where
    spiral' n line@(Line p1 p2)
      | n <= 0    = []
      | otherwise = p1 : spiral' (n - 1) newLine
      where
        newLine :: Line
        newLine = connectLine line (scaleLine scaleFactor (rotateLine angle line))

-- Following functions are adapted from learning Haskell
rotateLine :: Float -> Line -> Line
rotateLine alpha (Line (Point x1 y1) (Point x2 y2))
  = Line (Point x1 y1) (Point (x' + x1) (y' + y1))
  where
      x0 = x2 - x1
      y0 = y2 - y1
      x' = x0 * cos alpha - y0 * sin alpha
      y' = x0 * sin alpha + y0 * cos alpha
      

fadeColour :: Colour -> Colour
fadeColour (Colour red green blue opacity)
  = Colour red green blue (opacity - 50)
    

scaleLine :: Float -> Line -> Line
scaleLine factor (Line (Point x1 y1) (Point x2 y2))
  = Line (Point x1 y1) (Point (x' + x1) (y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = factor * x0 
    y' = factor * y0


connectLine :: Line -> Line -> Line
connectLine (Line _ endPoint) secondLine = startLineFrom endPoint secondLine
  where startLineFrom :: Point -> Line -> Line
        startLineFrom startPoint@(Point x0 y0) (Line (Point xS yS) (Point xE yE))
            = Line startPoint (Point (x0 + xE - xS) (y0 + yE - yS))


-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "art.png" (drawPicture 3 art)
  



