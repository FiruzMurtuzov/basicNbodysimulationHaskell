module Main (main) where

import Graphics.Gloss

-- Define a constant for the maximum length of the trace
maxTraceLength :: Int
maxTraceLength = 200

data Body = Body
  { mass     :: Float
  , position :: Vector
  , velocity :: Vector
  , trace    :: [Vector]
  } deriving (Show, Eq)

g :: Float
g = 1000

gravity :: Body -> Body -> Vector
gravity body1 body2 =
  let (x1, y1) = position body1
      (x2, y2) = position body2
      dx = x2 - x1
      dy = y2 - y1
      distSq = dx*dx + dy*dy
      softening = 20 
      distSoft = sqrt (distSq + softening * softening)
      forceMag = g * mass body1 * mass body2 / (distSoft * distSoft)
      forceX = forceMag * dx / distSoft
      forceY = forceMag * dy / distSoft
  in (forceX, forceY)

-- Update one body given all bodies
updateBody :: Float -> [Body] -> Body -> Body
updateBody dt allBodies body =
  let netForce = foldr
                   (\other acc ->
                      if other == body
                        then acc
                        else let (fx, fy) = gravity body other
                                 (ax, ay) = acc
                             in (ax + fx, ay + fy))
                   (0, 0)
                   allBodies
      accelX = fst netForce / mass body
      accelY = snd netForce / mass body
      (vx, vy) = velocity body
      newVelX = vx + accelX * dt
      newVelY = vy + accelY * dt
      newVel = (newVelX, newVelY)
      (px, py) = position body
      newPosX = px + newVelX * dt
      newPosY = py + newVelY * dt
      newPos = (newPosX, newPosY)
      newTrace = take maxTraceLength (newPos : trace body)
  in body { velocity = newVel, position = newPos, trace = newTrace }

simulationStep :: Float -> [Body] -> [Body]
simulationStep dt bodies = map (updateBody dt bodies) bodies

makeBody :: Float -> Vector -> Vector -> Body
makeBody m p v = Body m p v [p]

bodyMass :: Float
bodyMass = 10.0
bodiesPerSide :: Int
bodiesPerSide = 20
canvasBoundary :: Float
canvasBoundary = 400.0


{- Uncomment here and delete other initilaBodies function to see
the movement of 2 bodies -}

initialBodies :: [Body]
initialBodies =
 [ makeBody 200 (0, 0) (-20, 0),  -- Central heavy body with a slight initial velocity
  makeBody 200 (0, 100) (20, 0) ]


-- Linearly interpolate between two colors.
interpolateColor :: Color -> Color -> Float -> Color
interpolateColor startColor endColor t =
  let (r1, g1, b1, a1) = rgbaOfColor startColor
      (r2, g2, b2, a2) = rgbaOfColor endColor
      r = r1 * (1 - t) + r2 * t
      g = g1 * (1 - t) + g2 * t
      b = b1 * (1 - t) + b2 * t
      a = a1 * (1 - t) + a2 * t
  in makeColor r g b a

-- Draw a single trace with a color gradient
drawGradientTrace :: Color -> Color -> Body -> Picture
drawGradientTrace startColor endColor b =
  let path = trace b
      segments = zipWith (\p1 p2 -> (p1, p2)) path (tail path)
      totalSegments = fromIntegral $ length segments
      gradientLines = pictures
        [ let t = fromIntegral i / totalSegments
              segmentColor = interpolateColor startColor endColor t
          in color segmentColor (line [p1, p2])
        | (i, (p1, p2)) <- zip [0..] segments
        ]
  in gradientLines

-- The main drawing function
drawWorld :: [Body] -> Picture
drawWorld bodies =
  let colorPairs =
        [ (yellow, red)
        , (cyan,  blue)
        , (light green, violet)
        ]
  in pictures
    [ pictures $ zipWith (\(c1, c2) b -> drawGradientTrace c1 c2 b) (cycle colorPairs) bodies
    , pictures $ map drawBody bodies
    ]
  where
    drawBody :: Body -> Picture
    drawBody (Body m (x, y) _ _) =
      let radius = 2.0
      in translate x y $ color white $ circleSolid radius

-- Main function to run the simulation
main :: IO ()
main = simulate
    (InWindow "N-Body Simulation" (800, 800) (10, 10))
    black
    60
    initialBodies
    drawWorld
    (\_ dt bodies -> simulationStep dt bodies)
