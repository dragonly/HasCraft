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

data GLStuff = GLStuff {
    brick :: GL.TextureObject,
    stone :: GL.TextureObject,
    sand :: GL.TextureObject,
    grass_top :: GL.TextureObject,
    grass_bot :: GL.TextureObject,
    grass_sid :: GL.TextureObject
}
data TexIndex = BRICK | STONE | SAND | GRASS deriving (Eq, Show)

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
    shown :: World,
    mousePosLast :: (GL.GLint, GL.GLint),
    gravityAcceleration :: GL.GLfloat,
    player :: Player
}

makeInitState :: State
makeInitState = State {
    world = Map.fromList $  [((x,y,z), GRASS) | x <- [-10..10], z <- [-10..10], y <- [-1]] ++
                            [((x,y,z), STONE) | x <- [-10, 10], z <- [-10, 10], y <- [0..20]] ++ 
                            [((x,y,z), SAND ) | x <- [-10, -9, 9, 10], z <- [-10..10], y <- [-1]] ++ 
                            [((x,y,z), SAND ) | x <- [-10..10], z <- [-10, -9, 9, 10], y <- [-1]] ++ 
                            [((x,y,z), STONE) | x <- [-8..(-4)], z <- [-2..2], y <- [0]] ++
                            [((x,y,z), STONE) | x <- [-2..2], z <- [-2..2], y <- [0]] ++
                            [((x,y,z), STONE) | x <- [-8..(-4)], z <- [-8..(-4)], y <- [0]] ++
                            [((x,y,z), STONE) | x <- [-2..2], z <- [-8..(-4)], y <- [0]] ++
                            [((x,y,z), GRASS) | x <- [-7..1], z <- [-7..1], y <- [1]] ++
                            [((x,y,z), STONE) | x <- [-6..0], z <- [-6..0], y <- [2]] ++
                            [((x,y,z), GRASS) | x <- [-5..(-1)], z <- [-5..(-1)], y <- [3]] ++
                            [((x,y,z), STONE) | x <- [-4..(-2)], z <- [-4..(-2)], y <- [4]] ++
                            [((x,y,z), GRASS) | x <- [-3], z <- [-3], y <- [5]] ++
                            [((x,y,z), BRICK) | x <- [-10, -9, 9, 10], z <- [-10, -9, 9, 10], y <- [21]] ++
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
    mousePosLast = (0, 0),
    gravityAcceleration = 9.8,
    player = Player {
        rotation = (0, 0),
        vx = 0.0,
        vy = 0.0,
        vz = 0.0,
        jump = True,
        eye = GL.Vertex3 0 10 0
    }
}

initGL :: IO GLStuff
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
    return $ GLStuff {
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

render :: State -> GLStuff -> IORef GLfloat -> IO ()
render state stuff angle = do
    let state0 = state
        player0 = player state0
        (alpha, beta) = rotation player0
        eye0 = eye player0
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    -- ModelView
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    let (x, y, z) = getSightVector (alpha, beta)
        target = (+) <$> eye0 <*> GL.Vertex3 x y z

    GLU.lookAt eye0 target (GL.Vector3 0.0 1.0 0.0)
    
    -- render stone floor
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

    -- render THREE BODIES
    a <- get angle

    GL.preservingMatrix $ do
        GL.rotate a $ GL.Vector3 1 0 (0::GL.GLfloat)
        --GL.scale 0.7 0.7 (0.7::GL.GLfloat)

        GL.translate $ (GL.Vector3 0 20 0 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
        drawCubeSide
        GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
        drawCubeTop
        GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
        drawCubeBot
    GL.preservingMatrix $ do
        GL.rotate (a-90) $ GL.Vector3 1 0 (0::GL.GLfloat)
        --GL.scale 0.7 0.7 (0.7::GL.GLfloat)

        GL.translate $ (GL.Vector3 0 20 0 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
        drawCubeSide
        GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
        drawCubeTop
        GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
        drawCubeBot
    GL.preservingMatrix $ do
        GL.rotate (a-180) $ GL.Vector3 1 0 (0::GL.GLfloat)
        --GL.scale 0.7 0.7 (0.7::GL.GLfloat)

        GL.translate $ (GL.Vector3 0 22 0 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
        drawCubeSide
        GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
        drawCubeTop
        GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
        drawCubeBot
    GL.preservingMatrix $ do
        GL.rotate (a-270) $ GL.Vector3 1 0 (0::GL.GLfloat)
        --GL.scale 0.7 0.7 (0.7::GL.GLfloat)

        GL.translate $ (GL.Vector3 0 22 0 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
        drawCubeSide
        GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
        drawCubeTop
        GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
        drawCubeBot

    GL.preservingMatrix $ do
        GL.rotate (a-45) $ GL.Vector3 0 0 (1::GL.GLfloat)
        --GL.scale 0.7 0.7 (0.7::GL.GLfloat)

        GL.translate $ (GL.Vector3 0 20 0 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
        drawCubeSide
        GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
        drawCubeTop
        GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
        drawCubeBot
    GL.preservingMatrix $ do
        GL.rotate (a-135) $ GL.Vector3 0 0 (1::GL.GLfloat)
        --GL.scale 0.7 0.7 (0.7::GL.GLfloat)

        GL.translate $ (GL.Vector3 0 20 0 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
        drawCubeSide
        GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
        drawCubeTop
        GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
        drawCubeBot
    GL.preservingMatrix $ do
        GL.rotate (a-225) $ GL.Vector3 0 0 (1::GL.GLfloat)
        --GL.scale 0.7 0.7 (0.7::GL.GLfloat)

        GL.translate $ (GL.Vector3 0 22 0 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
        drawCubeSide
        GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
        drawCubeTop
        GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
        drawCubeBot
    GL.preservingMatrix $ do
        GL.rotate (a-315) $ GL.Vector3 0 0 (1::GL.GLfloat)
        --GL.scale 0.7 0.7 (0.7::GL.GLfloat)

        GL.translate $ (GL.Vector3 0 22 0 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
        drawCubeSide
        GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
        drawCubeTop
        GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
        drawCubeBot

    --drawAxis

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
            | keys!!4 == GLFW.Press = 5.2
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

applyMovement :: State -> GL.GLfloat -> State
applyMovement state dt =
    let
        world0 = world state
        player' = player state
        (alpha, beta) = rotation player'
        dx0 = (vx player') * (realToFrac dt)
        dy0 = (vy player') * (realToFrac dt)
        dz0 = (vz player') * (realToFrac dt)
        dy = dy0
        dz = dz0 * (cos alpha) + dx0 * (sin alpha)
        dx = (-dz0) * (sin alpha) + dx0 * (cos alpha)
        GL.Vertex3 eyeX eyeY eyeZ = eye player'
        GL.Vertex3 posX posY posZ = GL.Vertex3 (eyeX - 0.45) (eyeY - 0.45) (eyeZ - 0.45)
        GL.Vertex3 dx' dy' dz' = collide world0 (GL.Vertex3 posX posY posZ) (GL.Vertex3 (realToFrac dx) (realToFrac dy) (realToFrac dz))
        GL.Vertex3 eyeX' eyeY' eyeZ' = GL.Vertex3 (posX + dx' + 0.45) (posY + dy' + 0.45) (posZ + dz' + 0.45)
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

update :: State -> GL.GLfloat -> IORef GL.GLfloat -> IO State
update state dt angle = do
    Position mouseX mouseY <- get GLFW.mousePos
    w <- GLFW.getKey 'W'
    s <- GLFW.getKey 'S'
    a <- GLFW.getKey 'A'
    d <- GLFW.getKey 'D'
    space <- GLFW.getKey ' '
    let state0 = state
        (mousePosLastX, mousePosLastY) = mousePosLast state0
        player0 = player state0
        GL.Vertex3 x y z = eye player0
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

        stateAfterKeys = processKeyPress state0 [w, s, a, d, space]
        stateAfterGravity = applyGravity stateAfterKeys dt
        state' = applyMovement stateAfterGravity dt
        --state' = stateAfterGravity
        player' = player state'
        --vxx = (vx.player) stateAfterKeys
        --vyy = (vy.player) stateAfterKeys
        --vzz = (vz.player) stateAfterKeys
    
    --print $ (vxx, vyy, vzz)
    --print $ (jump.player) stateAfterKeys
    return state' {
        player = player' { rotation = rotation' },
        mousePosLast = (mouseX, mouseY)
    }

loop :: State -> GLStuff -> Float -> IORef GLfloat -> Int -> IO ()
loop state stuff lastTime angle countDown = do
    -- dt
    nowD <- get time
    let now = realToFrac nowD
    let dt = realToFrac $ now - lastTime

    -- game
    newState <- Main.update state dt angle
    render newState stuff angle
    angle $~! (+ 0.3)
    
    if countDown == 0
        then putStrLn $ "FPS: " ++ (show $ 1/dt)
        else return ()

    -- exit if window closed or Esc pressed
    esc <- GLFW.getKey GLFW.ESC
    q <- GLFW.getKey 'Q'
    open <- GLFW.getParam GLFW.Opened
    if open && esc /= GLFW.Press && q /= GLFW.Press
        then loop newState stuff now angle ((countDown-1) `mod` 60)
        --then loop state stuff now angle
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
    GL.fogColor $= GL.Color4 0.5 0.69 1.0 1
    GL.fogMode $= GL.Linear 5 60

    -- main loop
    now <- get GLFW.time
    angle <- newIORef 0
    loop state stuff (realToFrac now) angle 60
    
    GLFW.closeWindow
    GLFW.terminate