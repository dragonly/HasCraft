{-# LANGUAGE ScopedTypeVariables #-}

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
--import Graphics.DynamicGraph.Aixs
import Control.Monad
import Data.Map as Map
import Data.IORef as IORef
import Data.Fixed

import Etc

data GLStuff = GLStuff {
    brick  :: GL.TextureObject,
    stone :: GL.TextureObject,
    sand    :: GL.TextureObject,
    grass_top :: GL.TextureObject,
    grass_bot :: GL.TextureObject,
    grass_sid :: GL.TextureObject
}
data TexIndex = BRICK | STONE | SAND | GRASS deriving (Eq, Show)

data Player = Player {
    flying :: Bool,
    strafe :: (Int, Int),
    rotation :: (Float, Float),
    sector :: (Int, Int, Int),
    dy :: Float,
    inventory :: [TexIndex],
    block :: TexIndex,
    eye :: GL.Vertex3 GL.GLdouble
}

data State = State {
    world :: Map (Int, Int, Int) TexIndex,
    shown :: Map (Int, Int, Int) TexIndex,
    sectors :: Map (Int, Int, Int) [(Int, Int, Int)],
    mousePosLast :: (GL.GLint, GL.GLint),
    player :: Player
}

makeInitState :: State
makeInitState = State {
    world = Map.fromList $ [((x,y,z), STONE) | x <- [-8..8], z <- [-8..8], y <- [-2]] ++ [((x,y,z), GRASS) | x <- [-8..8], z <- [-8..8], y <- [-1]],
    shown = Map.fromList [],
    sectors = fromList [],
    mousePosLast = (0, 0),
    player = Player {
        flying = False,
        strafe = (0, 0),
        rotation = (0, 0),
        sector = (0, 0, 0),
        dy = 0.0,
        inventory = [GRASS, STONE, SAND],
        block = STONE,
        eye = GL.Vertex3 0 10 10
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
    GLU.perspective 65.0 aspect 1.0 60.0
    GL.matrixMode $= GL.Modelview 0
    return ()

drawAxis = do
    GL.preservingMatrix $ do
        GL.color $ GL.Color3 255 0 (0::GL.GLfloat)
        GL.renderPrimitive Lines $
            mapM_ (\(x,y,z) -> GL.vertex $ GL.Vertex3 x y (z::GL.GLfloat))
                [(0,0,0),(100,0,0),(0,0,0),(0,100,0),(0,0,0),(0,0,100)]

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

        GL.translate $ (GL.Vector3 0 3 0 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
        drawCubeSide
        GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
        drawCubeTop
        GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
        drawCubeBot
    GL.preservingMatrix $ do
        GL.rotate (a-1) $ GL.Vector3 0 1 (0::GL.GLfloat)
        --GL.scale 0.7 0.7 (0.7::GL.GLfloat)

        GL.translate $ (GL.Vector3 0 0 3 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (brick stuff)
        drawCubeSide
        drawCubeTop
        drawCubeBot
    GL.preservingMatrix $ do
        GL.rotate (a-2) $ GL.Vector3 0 0 (1::GL.GLfloat)
        --GL.scale 0.7 0.7 (0.7::GL.GLfloat)

        GL.translate $ (GL.Vector3 3 0 0 :: GL.Vector3 GL.GLfloat)

        GL.textureBinding GL.Texture2D $= Just (sand stuff)
        drawCubeSide
        drawCubeTop
        drawCubeBot

    --drawAxis

    GLFW.swapBuffers

getSightVector (alpha, beta) =
    let x = realToFrac $ (cos beta) * (sin alpha)
        y = realToFrac $ sin beta
        z = realToFrac $ (cos beta) * (-(cos alpha))
    in (x,y,z)

getCrossProduct (x1, y1, z1) (x2, y2, z2) = 
    let x = (y1 * z2) - (y2 * z1)
        y = (z1 * x2) - (z2 * x1)
        z = (x1 * y2) - (x2 * y1)
    in
        (x, y, z)

processMotion :: State -> GLFW.KeyButtonState -> GL.Vertex3 GL.GLdouble -> State
processMotion state key delta = 
    let player' = player state
        eye0 = eye player'
        eye' = (+) <$> eye0 <*> delta
    in
        if key == GLFW.Press
            then
                state { player = player' { eye = eye' } }
            else
                state

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
        player' = player state0
        GL.Vertex3 x y z = eye player'
        (alpha0, beta0) = rotation player'

        dx = mouseX - mousePosLastX
        dy = mouseY - mousePosLastY
        deltaAlpha = (fromIntegral dx) * pi / 400 /(realToFrac dt)/60
        deltaBeta = (fromIntegral dy) * pi/2 /300 /(realToFrac dt)/60
        --beta = -(max (-(pi/2)) $ min (pi/2) beta')
        alpha = (alpha0 + deltaAlpha) `mod'` (2*pi)
        beta = (max (-(pi/2)+0.0000001) $ min (pi/2-0.0000001) (beta0 - deltaBeta))
        rotation' = (alpha,beta)
        (tgX, tgY, tgZ) = getSightVector (alpha, beta)
        (crossX, crossY, crossZ) = getCrossProduct (0, 1, 0) (tgX, tgY, tgZ)

        motionScale = 0.2 /(realToFrac dt)/60
        deltaW = GL.Vertex3 (tgX*motionScale) (tgY*motionScale) (tgZ*motionScale)
        deltaS = GL.Vertex3 (-(tgX*motionScale)) (-(tgY*motionScale)) (-(tgZ*motionScale))
        deltaA = GL.Vertex3 (crossX*motionScale) (crossY*motionScale) (crossZ*motionScale)
        deltaD = GL.Vertex3 (-(crossX*motionScale)) (-(crossY*motionScale)) (-(crossZ*motionScale))
        deltaSPACE = GL.Vertex3 0 0.1 0

        stateW = processMotion state0 w deltaW
        stateS = processMotion stateW s deltaS
        stateA = processMotion stateS a deltaA
        stateD = processMotion stateA d deltaD
        stateSPACE = processMotion stateD space deltaSPACE
        state' = stateSPACE
        eye' = (eye.player) state'

    --print (alpha/pi*180, beta/pi*180)
    return state { player = player' { rotation = rotation',
                                      eye = eye'
                                    },
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
    angle $~! (+ 0)
    
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
    GLFW.windowTitle        $= "Test"
    GLFW.windowSizeCallback $= resize
    --GLFW.mousePos $= Position 0 0
    GLFW.disableSpecial GLFW.MouseCursor

    -- main loop
    now <- get GLFW.time
    angle <- newIORef 0
    loop state stuff (realToFrac now) angle 60
    
    GLFW.closeWindow
    GLFW.terminate