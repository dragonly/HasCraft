{-# LANGUAGE ScopedTypeVariables #-}

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Control.Monad
import Data.Map as Map
import Data.IORef as IORef
import Data.Fixed

import Etc

data GLTexture = GLTexture {
    brick :: GL.TextureObject,
    stone :: GL.TextureObject,
    sand :: GL.TextureObject,
    grass_top :: GL.TextureObject,
    grass_bot :: GL.TextureObject,
    grass_sid :: GL.TextureObject
}
data TexIndex = BRICK | STONE | SAND | GRASS deriving (Eq, Show)

-- special position for teleportation
portal1 :: ((Int, Int, Int), (Int, Int, Int))
portal1 = ((8, (-14), (-3)), (15, 101+4, 0))
portal2 :: ((Int, Int, Int), (Int, Int, Int))
portal2 = ((24, 101, 0), ((-3), (-14)+4, (-3)))

data Player = Player {
    rotation :: (Float, Float),
    vx :: Float,
    vy :: Float,
    vz :: Float,
    jump :: Bool,
    eye :: GL.Vertex3 GL.GLdouble
}

type World = Map (Int, Int, Int) TexIndex
data State = State {
    world :: World,
    shown :: World, -- for optimization, not used yet
    focus :: (Int, Int, Int),
    countDown :: Int,
    mousePosLast :: (GL.GLint, GL.GLint),
    gravityAcceleration :: GL.GLfloat,
    player :: Player
}

makeInitState :: State
makeInitState = State {
    world = Map.fromList $  [((x,y,z), GRASS) | x <- [-10..(-4)], z <- [-10..10], y <- [-1]] ++ --Floor
                            [((x,y,z), GRASS) | x <- [-2..10], z <- [-10..10], y <- [-1]] ++
                            [((x,y,z), GRASS) | x <- [-3], z <- [-10..(-7)], y <- [-1]] ++
                            [((x,y,z), GRASS) | x <- [-3], z <- [1..10], y <- [-1]] ++
                            [((x,y,z), GRASS) | x <- [-3], z <- [-3], y <- [-1]] ++
                            [((x,y,z), BRICK) | x <- [-3], z <- [-6], y <- [-2]] ++ --Stairs
                            [((x,y,z), BRICK) | x <- [-3], z <- [-4], y <- [-2]] ++
                            [((x,y,z), BRICK) | x <- [-3], z <- [-2], y <- [-2]] ++
                            [((x,y,z), BRICK) | x <- [-3], z <- [0], y <- [-2]] ++
                            [((x,y,z), BRICK) | (x, z, y) <- zip3 [-9..(-3)] (repeat (-1)) [-9..(-3)]] ++
                            [((x,y,z), BRICK) | (x, z, y) <- zip3 [-3..3] (repeat (-1)) [-3, (-4)..(-9)]] ++
                            [((x,y,z), BRICK) | (x, z, y) <- zip3 [-9..(-3)] (repeat (-5)) [-9..(-3)]] ++
                            [((x,y,z), BRICK) | (x, z, y) <- zip3 [-3..3] (repeat (-5)) [-3, (-4)..(-9)]] ++
                            [((x,y,z), GRASS) | x <- [-12..(-10)], z <- [-10..6], y <- [-9]] ++ --Platforms
                            [((x,y,z), GRASS) | x <- [4..6], z <- [-10..6], y <- [-9]] ++
                            [((x,y,z), BRICK) | (x, z, y) <- zip3 [-9..(-3)] (repeat 0) [-9, (-10)..(-15)]] ++ --Stairs2
                            [((x,y,z), BRICK) | (x, z, y) <- zip3 [-3..3] (repeat (0)) [-15..(-9)]] ++
                            [((x,y,z), BRICK) | (x, z, y) <- zip3 [-9..(-3)] (repeat (-6)) [-9, (-10)..(-15)]] ++
                            [((x,y,z), BRICK) | (x, z, y) <- zip3 [-3..3] (repeat (-6)) [-15..(-9)]] ++
                            [((x,y,z), GRASS) | x <- [-6..0], z <- [-6..0], y <- [-15]] ++ --Platforms2
                            [((x,y,z), GRASS) | x <- [1..6], z <- [-5..(-1)], y <- [-15]] ++
                            [((x,y,z), GRASS) | x <- [7], z <- [-4..(-2)], y <- [-15]] ++
                            [((x,y,z), GRASS) | x <- [8], z <- [-3], y <- [-15]] ++
                            [((x,y,z), STONE) | x <- [7], z <- [-4, (-2)], y <- [-14, (-13)]] ++ --Portal1
                            [((x,y,z), STONE) | x <- [7], z <- [-4..(-2)], y <- [-12]] ++
                            [((x,y,z), STONE) | x <- [-10, 10], z <- [-10, 10], y <- [0..20]] ++ --Columns
                            [((x,y,z), SAND ) | x <- [-10, -9, 9, 10], z <- [-10..10], y <- [-1]] ++ --Sides
                            [((x,y,z), SAND ) | x <- [-10..10], z <- [-10, -9, 9, 10], y <- [-1]] ++ 
                            [((x,y,z), STONE) | x <- [-8..(-4)], z <- [-2..2], y <- [0]] ++ --Tower
                            [((x,y,z), STONE) | x <- [-2..2], z <- [-2..2], y <- [0]] ++
                            [((x,y,z), STONE) | x <- [-8..(-4)], z <- [-8..(-4)], y <- [0]] ++
                            [((x,y,z), STONE) | x <- [-2..2], z <- [-8..(-4)], y <- [0]] ++
                            [((x,y,z), GRASS) | x <- [-7..1], z <- [-7..1], y <- [1]] ++
                            [((x,y,z), STONE) | x <- [-6..0], z <- [-6..0], y <- [2]] ++
                            [((x,y,z), GRASS) | x <- [-5..(-1)], z <- [-5..(-1)], y <- [3]] ++
                            [((x,y,z), STONE) | x <- [-4..(-2)], z <- [-4..(-2)], y <- [4]] ++
                            [((x,y,z), GRASS) | x <- [-3], z <- [-3], y <- [5]] ++
                            [((x,y,z), GRASS) | x <- [17..22], z <- [-2..2], y <- [100]] ++ --FaithPlat
                            [((x,y,z), GRASS) | x <- [16, 23], z <- [-1..1], y <- [100]] ++
                            [((x,y,z), GRASS) | x <- [15, 24], z <- [0], y <- [100]] ++
                            [((x,y,z), STONE) | x <- [23], z <- [-1, 1], y <- [101, 102]] ++ --Portal2
                            [((x,y,z), STONE) | x <- [23], z <- [-1..1], y <- [103]] ++
                            [((x,y,z), BRICK) | x <- [19, 20], z <- [3, 4], y <- [100]] ++ --FaithStair
                            [((x,y,z), BRICK) | x <- [20, 21], z <- [4, 5], y <- [99]] ++
                            [((x,y,z), BRICK) | x <- [21, 22], z <- [5, 6], y <- [98]] ++
                            [((x,y,z), BRICK) | x <- [22, 23], z <- [6, 7], y <- [97]] ++
                            [((x,y,z), BRICK) | x <- [23, 24], z <- [7, 8], y <- [96]] ++
                            [((x,y,z), BRICK) | x <- [24, 25], z <- [8, 9], y <- [95]] ++
                            [((x,y,z), BRICK) | x <- [25, 26], z <- [7, 8], y <- [94]] ++
                            [((x,y,z), BRICK) | x <- [26, 27], z <- [6, 7], y <- [93]] ++
                            [((x,y,z), BRICK) | x <- [27, 28], z <- [5, 6], y <- [92]] ++
                            [((x,y,z), BRICK) | x <- [28, 29], z <- [4, 5], y <- [91]] ++
                            [((x,y,z), BRICK) | x <- [29, 30], z <- [3, 4], y <- [90]] ++
                            [((x,y,z), GRASS) | x <- [26..33], z <- [0..7], y <- [89]] ++
                            [((x,y,z), BRICK) | x <- [-10, -9, 9, 10], z <- [-10, -9, 9, 10], y <- [21]] ++ --Roof
                            [((x,y,z), BRICK) | x <- [-9, -8, 8, 9], z <- [-9, -8, 8, 9], y <- [22]] ++
                            [((x,y,z), BRICK) | x <- [-8, -7, 7, 8], z <- [-8, -7, 7, 8], y <- [23]] ++
                            [((x,y,z), BRICK) | x <- [-7, -6, 6, 7], z <- [-7, -6, 6, 7], y <- [24]] ++
                            [((x,y,z), BRICK) | x <- [-6, -5, 5, 6], z <- [-6, -5, 5, 6], y <- [25]] ++
                            [((x,y,z), BRICK) | x <- [-5, -4, 4, 5], z <- [-5, -4, 4, 5], y <- [26]] ++
                            [((x,y,z), BRICK) | x <- [-4, -3, 3, 4], z <- [-4, -3, 3, 4], y <- [27]] ++
                            [((x,y,z), BRICK) | x <- [-3, -2, 2, 3], z <- [-3, -2, 2, 3], y <- [28]] ++
                            [((x,y,z), BRICK) | x <- [-2, -1, 1, 2], z <- [-2, -1, 1, 2], y <- [29]] ++
                            [((x,y,z), BRICK) | x <- [-1..1], z <- [-1..1], y <- [29]],
    shown = Map.fromList [],
    focus = (999, 999, 999),
    countDown = 60,
    mousePosLast = (0, 0),
    gravityAcceleration = 20,
    player = Player {
        rotation = (0, 0),
        vx = 0.0,
        vy = 0.0,
        vz = 0.0,
        jump = True,
        eye = GL.Vertex3 28 94 2
    }
}

initGL :: IO GLTexture
initGL = do
    GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
    GL.depthFunc $= Just GL.Lequal
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.normalize $= GL.Enabled
    GL.texture GL.Texture2D $= GL.Enabled

    -- reset matrixes
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    -- load textures
    brick <- loadTexture "texture/brick.tga"
    stone <- loadTexture "texture/stone.tga"
    sand <- loadTexture "texture/sand.tga"
    grass_top <- loadTexture "texture/grass_top.tga"
    grass_bot <- loadTexture "texture/grass_bot.tga"
    grass_sid <- loadTexture "texture/grass_sid.tga"

    -- make stuff
    return $ GLTexture {
        brick = brick,
        stone = stone,
        sand = sand,
        grass_top = grass_top,
        grass_bot = grass_bot,
        grass_sid = grass_sid
    }

resize :: GLFW.WindowSizeCallback
resize size@(Size w h) = do
    let hh = if h < 0 then 1 else h
    let aspect = (fromIntegral w) / (fromIntegral hh)
    GL.viewport   $= (Position 0 0, size)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GLU.perspective 65.0 aspect 0.01 60.0
    GL.matrixMode $= GL.Modelview 0
    return ()

--drawAxis = do
--    GL.preservingMatrix $ do
--        GL.color $ GL.Color3 255 0 (0::GL.GLfloat)
--        GL.renderPrimitive Lines $
--            mapM_ (\(x,y,z) -> GL.vertex $ GL.Vertex3 x y (z::GL.GLfloat))
--                [(0,0,0),(100,0,0),(0,0,0),(0,100,0),(0,0,0),(0,0,100)]

-- design `focus` field to draw a frame on the focus block, not implemented yet
putBlock state =
    let
        focusPos = focus state
        world0 = world state
        world' = Map.insert focusPos BRICK world0
        countDown0 = countDown state
    in
        if (countDown0 `mod` 10) == 0
            then state {
                     world = world'
                 }
            else
                state

putStuff :: TexIndex -> GLTexture -> GL.Vector3 GL.GLfloat -> GLfloat -> GL.Vector3 GL.GLfloat -> GL.Vector3 GL.GLfloat -> IO ()
putStuff texture stuff translateBeforeRotateVector rotateAngle rotateVector translateAfterRotateVector = do
    GL.preservingMatrix $ do
        GL.translate $ translateBeforeRotateVector

        --rotateAngle <- get angle

        GL.rotate rotateAngle $ rotateVector

        GL.translate $ translateAfterRotateVector

        case texture of
            BRICK -> GL.textureBinding GL.Texture2D $= Just (brick stuff)
            STONE -> GL.textureBinding GL.Texture2D $= Just (stone stuff)
            SAND -> GL.textureBinding GL.Texture2D $= Just (sand stuff)
            GRASS -> GL.textureBinding GL.Texture2D $= Just (grass_sid stuff) 

        if texture == BRICK || texture == STONE || texture == SAND
            then do
                drawCubeSide
                drawCubeTop
                drawCubeBot
            else do
                drawCubeSide
                GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
                drawCubeTop
                GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
                drawCubeBot
    return ()    

render :: State -> GLTexture -> IORef GLfloat -> IO ()
render state stuff angle = do
    let state0 = state
        player0 = player state0
        (alpha, beta) = rotation player0
        eye0 = eye player0
        (focusX, focusY, focusZ) = focus state0
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    -- ModelView
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    let (x, y, z) = getSightVector (alpha, beta)
        target = (+) <$> eye0 <*> GL.Vertex3 x y z

    GLU.lookAt eye0 target (GL.Vector3 0.0 1.0 0.0)
    
    -- render world
    forM_ (Map.toList $ world state0) $ \((x,y,z), textureIndex) ->
        GL.preservingMatrix $ do
            GL.translate $ (GL.Vector3 (realToFrac x) (realToFrac y) (realToFrac z) :: GL.Vector3 GL.GLfloat)
            case textureIndex of
                BRICK -> GL.textureBinding GL.Texture2D $= Just (brick stuff)
                STONE -> GL.textureBinding GL.Texture2D $= Just (stone stuff)
                SAND -> GL.textureBinding GL.Texture2D $= Just (sand stuff)
                GRASS -> GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
            if textureIndex == BRICK || textureIndex == STONE || textureIndex == SAND
                then do
                    drawCubeSide
                    drawCubeTop
                    drawCubeBot
                else do
                    drawCubeSide
                    GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
                    drawCubeTop
                    GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
                    drawCubeBot
    -- render focus frame
    --GL.preservingMatrix $ do
    --    GL.translate $ (GL.Vector3 (realToFrac focusX) (realToFrac focusY) (realToFrac focusZ) :: GL.Vector3 GL.GLfloat)
    --    GL.scale 1.06 1.06 (1.06 :: GLfloat)
    --    drawFrame

    rotateAngle <- get angle

    putStuff GRASS stuff (GL.Vector3 0 20 0 :: GL.Vector3 GL.GLfloat) rotateAngle (GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat) (GL.Vector3 0 5 0 :: GL.Vector3 GL.GLfloat)
    putStuff GRASS stuff (GL.Vector3 0 20 0 :: GL.Vector3 GL.GLfloat) rotateAngle (GL.Vector3 0 1 0 :: GL.Vector3 GL.GLfloat) (GL.Vector3 0 0 5 :: GL.Vector3 GL.GLfloat)
    putStuff GRASS stuff (GL.Vector3 0 20 0 :: GL.Vector3 GL.GLfloat) rotateAngle (GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat) (GL.Vector3 5 0 0 :: GL.Vector3 GL.GLfloat)

    putStuff GRASS stuff (GL.Vector3 0 12 0 :: GL.Vector3 GL.GLfloat) (2*rotateAngle) (GL.Vector3 0.3 1 0 :: GL.Vector3 GL.GLfloat) (GL.Vector3 20 0 0 :: GL.Vector3 GL.GLfloat)
    putStuff GRASS stuff (GL.Vector3 0 15 0 :: GL.Vector3 GL.GLfloat) (6*rotateAngle) (GL.Vector3 (-0.3) 1 0 :: GL.Vector3 GL.GLfloat) (GL.Vector3 28 0 0 :: GL.Vector3 GL.GLfloat)
    putStuff GRASS stuff (GL.Vector3 0 18 0 :: GL.Vector3 GL.GLfloat) (-4*rotateAngle) (GL.Vector3 0.15 1 0 :: GL.Vector3 GL.GLfloat) (GL.Vector3 24 0 0 :: GL.Vector3 GL.GLfloat)

    GLFW.swapBuffers

getSightVector (alpha, beta) =
    let x = realToFrac $ (cos beta) * (sin alpha)
        y = realToFrac $ sin beta
        z = realToFrac $ (cos beta) * (-(cos alpha))
    in (x,y,z)

getMoveVector (x, y, z) =
    let 
        norm = sqrt(x^2 + z^2)
    in
        (x/norm, 0, z/norm)

getCrossProduct (x1, y1, z1) (x2, y2, z2) = 
    let x = (y1 * z2) - (y2 * z1)
        y = (z1 * x2) - (z2 * x1)
        z = (x1 * y2) - (x2 * y1)
    in
        (x, y, z)

applyGravity :: State -> GL.GLfloat -> State
applyGravity state dt = 
    let
        g = gravityAcceleration state
        player' = player state
        vy0 = vy player'
        dvy = g * dt
        vy' = max (-10) (vy0 - (realToFrac dvy))
    in
        state {
            player = player' {
                vy = vy'
            }
        }

processKeyPress :: State -> [GLFW.KeyButtonState] -> State --[w,s,a,d,space]
processKeyPress state keys =
    let
        player' = player state
        jump0 = jump player'
        vx0 = vx player'
        vy0 = vy player'
        vz0 = vz player'
        countDown0 = countDown state
        vz'
            | keys!!0 == GLFW.Press && keys!!1 == GLFW.Release = (-2)
            | keys!!0 == GLFW.Release && keys!!1 == GLFW.Press = 2
            | otherwise = 0
        vx'
            | keys!!2 == GLFW.Press && keys!!3 == GLFW.Release = (-2)
            | keys!!2 == GLFW.Release && keys!!3 == GLFW.Press = 2
            | otherwise = 0
        -- jump
        vy'
            | keys!!4 == GLFW.Press && (countDown0 `mod` 10) == 0 = 7.4 -- jump speed
            | otherwise = vy0
        --vy' = vspace
    in
        if jump0 == True
            then
                state
            else
                state {
                    player = player' {
                        vx = vx',
                        vy = vy',
                        vz = vz'
                    }
                }

-- return position just before collision
collide :: World -> GL.Vertex3 GL.GLdouble -> GL.Vertex3 GL.GLdouble -> GL.Vertex3 GL.GLdouble
collide world position movement =
    let
        GL.Vertex3 posX0 posY0 posZ0 = position
        GL.Vertex3 dx dy dz = movement
        blockSize = 0.9
        --(posX, posY, posZ) = (floor posX0, floor posY0, floor posZ0)
        --(resX, resY, resZ) = (mod' posX0 1, mod' posY0 1, mod' posZ0 1)
        -- determine which 3 faces will probably collide with the world
        -- then apply collision detection on 3 directions respectively
        faceX
            | dx > 0 = [(posX0+blockSize,posY0,posZ0),(posX0+blockSize,posY0+blockSize,posZ0),(posX0+blockSize,posY0+blockSize,posZ0+blockSize),(posX0+blockSize,posY0,posZ0+blockSize)]
            | dx < 0 = [(posX0,posY0,posZ0),  (posX0,posY0+blockSize,posZ0),  (posX0,posY0+blockSize,posZ0+blockSize),  (posX0,posY0,posZ0+blockSize)]
            | otherwise = []
        faceY
            | dy > 0 = [(posX0,posY0+blockSize,posZ0),(posX0,posY0+blockSize,posZ0+blockSize),(posX0+blockSize,posY0+blockSize,posZ0+blockSize),(posX0+blockSize,posY0+blockSize,posZ0)]
            | dy < 0 = [(posX0,posY0,posZ0),  (posX0,posY0,posZ0+blockSize),  (posX0+blockSize,posY0,posZ0+blockSize),  (posX0+blockSize,posY0,posZ0)]
            | otherwise = []
        faceZ
            | dz > 0 = [(posX0,posY0,posZ0+blockSize),(posX0+blockSize,posY0,posZ0+blockSize),(posX0+blockSize,posY0+blockSize,posZ0+blockSize),(posX0,posY0+blockSize,posZ0+blockSize)]
            | dz < 0 = [(posX0,posY0,posZ0),  (posX0+blockSize,posY0,posZ0),  (posX0+blockSize,posY0+blockSize,posZ0),  (posX0,posY0+blockSize,posZ0)]
            | otherwise = []
        dx' = actualMove world faceX dx dx 'x'
        dy' = actualMove world faceY dy dy 'y'
        dz' = actualMove world faceZ dz dz 'z'
    in
        GL.Vertex3 dx' dy' dz'

actualMove :: World -> [(GL.GLdouble, GL.GLdouble, GL.GLdouble)] -> GL.GLdouble -> GL.GLdouble -> Char -> GL.GLdouble
actualMove world vertexs disEnd disAll direction = 
    let
        vertexsPost
            | direction == 'x' = Prelude.map (\(x,y,z) -> (x+disEnd, y, z)) vertexs
            | direction == 'y' = Prelude.map (\(x,y,z) -> (x, y+disEnd, z)) vertexs
            | direction == 'z' = Prelude.map (\(x,y,z) -> (x, y, z+disEnd)) vertexs
    in
        if checkCollision world vertexsPost then actualMove world vertexs (disEnd - disAll/10) disAll direction else disEnd

checkCollision :: Map (Int, Int, Int) TexIndex -> [(GL.GLdouble, GL.GLdouble, GL.GLdouble)] -> Bool
checkCollision world positions =
    let
        positionsFloor = Prelude.map (\(x,y,z) -> (floor x, floor y, floor z)) positions
        lookups = fmap (\k -> Map.lookup k world) positionsFloor
        result = Prelude.foldr (\x y -> if x == Nothing && y == Nothing then Nothing else Just 1) Nothing lookups
    in
        if result == Nothing then False else True

-- apply movement with regards to velocity player gains in this frame
applyMovement :: State -> GL.GLfloat -> State
applyMovement state dt =
    let
        world0 = world state
        player' = player state
        (alpha, beta) = rotation player'
        -- calculate delta 
        dx0 = (vx player') * (realToFrac dt)
        dy0 = (vy player') * (realToFrac dt)
        dz0 = (vz player') * (realToFrac dt)
        dy = dy0
        dz = dz0 * (cos alpha) + dx0 * (sin alpha)
        dx = (-dz0) * (sin alpha) + dx0 * (cos alpha)
        GL.Vertex3 eyeX eyeY eyeZ = eye player'
        GL.Vertex3 posX posY posZ = GL.Vertex3 (eyeX - 0.45) (eyeY - 0.9) (eyeZ - 0.45)
        GL.Vertex3 dx' dy' dz' = collide world0 (GL.Vertex3 posX posY posZ) (GL.Vertex3 (realToFrac dx) (realToFrac dy) (realToFrac dz))
        GL.Vertex3 eyeX' eyeY' eyeZ' = GL.Vertex3 (posX + dx' + 0.45) (posY + dy' + 0.9) (posZ + dz' + 0.45)
        jump'
            | abs(dy') < 0.000001 = False
            | otherwise = True
        vy'
            | jump' == False = 0
            | otherwise = vy player'
    in
        state {
            player = player' {
                eye = GL.Vertex3 eyeX' eyeY' eyeZ',
                jump = jump',
                vy = vy'
            }
        }

--hitTest :: State -> ((Int, Int, Int), (Int, Int, Int))
--hitTest state =
--    let
--        world0 = world state
--        GL.Vertex3 eyeX eyeY eyeZ = (eye.player) state
--        (alpha, beta) = (rotation.player) state
--        (sightX, sightY, sightZ) = getSightVector (alpha, beta)
--        (blockX, blockY, blockZ) = (floor eyeX, floor eyeY, floor eyeZ)
--        (key, prev) = _hitTest world0 (eyeX, eyeY, eyeZ) (sightX, sightY,sightZ) (blockX, blockY, blockZ) (realToFrac 1) 24
--    in
--        (key, prev)

--_hitTest world (eyeX, eyeY, eyeZ) (sightX, sightY, sightZ) (prevX, prevY, prevZ) step maxStep =
--    let
--        (testX, testY, testZ) = (eyeX+sightX*step/8, eyeY+sightX*step/8, eyeZ+sightX*step/8)
--        (blockX, blockY, blockZ) = (floor testX, floor testY, floor testZ)
--    in
--        -- if exceeded max tange then return
--        if step > maxStep then ((999,999,999), (prevX, prevY, prevZ))
--            -- haven't gone out of current block
--            else if (blockX, blockY, blockZ) == (prevX, prevY, prevZ) then _hitTest world (eyeX, eyeY, eyeZ) (sightX, sightY, sightZ) (prevX, prevY, prevZ) (step+1) maxStep
--            -- enter a new block, check if in world
--                else if Map.lookup (blockX, blockY, blockZ) world /= Nothing then ((blockX, blockY, blockZ), (prevX, prevY, prevZ))
--                    -- or go on another step test, make this new block the `previous block`
--                    else _hitTest world (eyeX, eyeY, eyeZ) (sightX, sightY, sightZ) (blockX, blockY, blockZ) (step+1) maxStep


--_hitTest :: World -> Float -> (Float, Float, Float) -> (Float, Float, Float) -> ((Int, Int, Int), (Int, Int, Int))
--_hitTest world step (prevX, prevY, prevZ) (posX, posY, posZ) (sightX, sightY, sightZ) =
--    let
--        factor = step / 8
--        (x, y, z) = (posX + sightX * factor, posY + sightY * factor, posZ + sightZ * factor)
--        pos = (floor x, floor y, floor z)
--        found = Map.lookup pos world
--    in
--        if factor > 4 then ((9999,9999,9999), (9999,9999,9999))
--            else if found /= Nothing then ((prevX, prevY, prevZ), pos)
--                else _hitTest world (step+1) pos (posX, posY, posZ) (sightX, sightY, sightZ)
hitTest state (eyeX, eyeY, eyeZ) (sightX, sightY, sightZ) =
    let
        (x, y, z) = (floor $ eyeX+sightX, floor $ eyeY+sightY, floor $ eyeZ+sightZ)
    in
        state {
            focus = (x, y, z)
        }

-- if in teleport point, then move to destination
checkTeleport state =
    let
        player' = player state
        GL.Vertex3 eyeX eyeY eyeZ = eye player'
        -- get player position of the near-origin-corner
        (posX, posY, posZ) = (floor (eyeX-0.45), floor (eyeY-0.9), floor (eyeZ-0.45))
        (p1Src, p1Dst) = portal1
        (p2Src, p2Dst) = portal2
        -- set portal destination, or do nothing
        (posX', posY', posZ')
            | (posX, posY, posZ) == p1Src = p1Dst
            | (posX, posY, posZ) == p2Src = p2Dst
            | otherwise = (posX, posY, posZ)
        (eyeX', eyeY', eyeZ') = ((realToFrac posX')+0.45, (realToFrac posY')+0.9, (realToFrac posZ')+0.45)
    in
        -- only apply movement when player is at the portal point
        if (posX, posY, posZ) == p1Src || (posX, posY, posZ) == p2Src
            then
                state {
                    player = player' {
                        eye = GL.Vertex3 eyeX' eyeY' eyeZ'
                    }
                }
            else
                state

-- update game state
update :: State -> GL.GLfloat -> IORef GL.GLfloat -> IO State
update state dt angle = do
    -- get key press state and mouse position
    Position mouseX mouseY <- get GLFW.mousePos
    w <- GLFW.getKey 'W'
    s <- GLFW.getKey 'S'
    a <- GLFW.getKey 'A'
    d <- GLFW.getKey 'D'
    space <- GLFW.getKey ' '
    leftMouse <- GLFW.getMouseButton GLFW.ButtonLeft

    let state0 = state
        (mousePosLastX, mousePosLastY) = mousePosLast state0
        player0 = player state0
        GL.Vertex3 eyeX eyeY eyeZ = eye player0
        (alpha0, beta0) = rotation player0

        -- get mouse movement
        mouseDx = mouseX - mousePosLastX
        mouseDy = mouseY - mousePosLastY
        -- apply to rotation
        deltaAlpha = (fromIntegral mouseDx) * pi / 400 /(realToFrac dt)/60
        deltaBeta = (fromIntegral mouseDy) * pi/2/ 300 /(realToFrac dt)/60
        -- uniform
        alpha = (alpha0 + deltaAlpha) `mod'` (2*pi)
        beta = (max (-(pi/2)+0.1) $ min (pi/2-0.1) (beta0 - deltaBeta))
        rotation' = (alpha,beta)

        -- get movement vectors
        (sightX, sightY, sightZ) = getSightVector (alpha, beta)
        (moveX, moveY, moveZ) = getMoveVector (sightX, sightY, sightZ)
        (crossX, crossY, crossZ) = getCrossProduct (0, 1, 0) (moveX, moveY, moveZ)

        -- apply all sorts of key press and gravity
        stateAfterTeleport = checkTeleport state0
        stateAfterKeys = processKeyPress stateAfterTeleport [w, s, a, d, space]
        stateAfterGravity = applyGravity stateAfterKeys dt
        stateAfterMovement = applyMovement stateAfterGravity dt
        --((blockX, blockY, blockZ), (prevX, prevY, prevZ)) = hitTest stateAfterMovement 
        stateAfterHitTest = hitTest stateAfterMovement (eyeX, eyeY, eyeZ) (sightX, sightY, sightZ)
        stateAfterPutBlock
            | leftMouse == GLFW.Press = putBlock stateAfterHitTest
            | otherwise = stateAfterHitTest


        state' = stateAfterPutBlock
        player' = player state'
    --    vxx = (vx.player) stateAfterKeys
    --    vyy = (vy.player) stateAfterKeys
    --    vzz = (vz.player) stateAfterKeys
    
    --print $ (vxx, vyy, vzz)
    --print $ (jump.player) stateAfterKeys
    return state' {
        player = player' { rotation = rotation' },
        mousePosLast = (mouseX, mouseY)
        --focus = ((blockX, blockY, blockZ), (prevX, prevY, prevZ))
    }

-- game loop
loop :: State -> GLTexture -> Float -> IORef GLfloat -> IO ()
loop state stuff lastTime angle = do
    -- dt
    nowD <- get time
    let now = realToFrac nowD
    let dt = realToFrac $ now - lastTime

    -- update game state then render to screen
    newState <- Main.update state dt angle
    render newState stuff angle

    --print $ (eye.player) newState

    angle $~! (+ 0.3)
    let countDown0 = countDown state
    if countDown0 == 0
        then putStrLn $ "FPS: " ++ (show $ 1/dt)
        else return ()

    -- exit if window closed or Esc pressed
    esc <- GLFW.getKey GLFW.ESC
    q <- GLFW.getKey 'Q'
    open <- GLFW.getParam GLFW.Opened
    if open && esc /= GLFW.Press && q /= GLFW.Press
        then loop (newState {countDown = ((countDown0-1) `mod` 60)}) stuff now angle
        else return ()

main :: IO ()
main = do
    -- initialize
    GLFW.initialize
    -- open window
    GLFW.openWindow (GL.Size 800 600) [GLFW.DisplayRGBBits 8 8 8,
                                       GLFW.DisplayAlphaBits 8,
                                       GLFW.DisplayDepthBits 24] GLFW.Window
    -- init
    let state = makeInitState
    -- capture initial position of mouse and use it as the reference point for calculation of rotation
    Position mouseX mouseY <- get GLFW.mousePos
    stuff <- initGL
    -- setup stuff
    GLFW.swapInterval       $= 1 -- vsync
    GLFW.windowTitle        $= "HasCraft"
    GLFW.windowSizeCallback $= resize
    GLFW.disableSpecial GLFW.MouseCursor

    -- set up sky and fog
    GL.clearColor $= GL.Color4 0.5 0.69 1.0 1
    GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear') 
    --GL.fogColor $= GL.Color4 0.5 0.69 1.0 1
    --GL.fogMode $= GL.Linear 5 60

    now <- get GLFW.time
    -- for satellite blocks revolution
    angle <- newIORef 0
    -- main loop
    loop state stuff (realToFrac now) angle
    
    GLFW.closeWindow
    GLFW.terminate