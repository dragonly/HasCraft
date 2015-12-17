{-# LANGUAGE ScopedTypeVariables #-}

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
--import Graphics.DynamicGraph.Aixs
import Control.Monad
import Data.Map as Map
import Data.IORef as IORef

import Etc

data GLStuff = GLStuff {
    brick  :: GL.TextureObject,
    stone :: GL.TextureObject,
    sand    :: GL.TextureObject,
    grass_top :: GL.TextureObject,
    grass_bot :: GL.TextureObject,
    grass_sid :: GL.TextureObject
}
data TexIndex = GRASS | STONE | SAND | GRASS_TOP | GRASS_BOT | GRASS_SID

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
    mouseCenter :: (GL.GLint, GL.GLint),
    player :: Player
}

makeInitState :: State
makeInitState = State {
    world = Map.fromList [],
    shown = Map.fromList [],
    sectors = fromList [],
    mouseCenter = (0, 0),
    player = Player {
        flying = False,
        strafe = (0, 0),
        rotation = (0.0, 0.0),
        sector = (0, 0, 0),
        dy = 0.0,
        inventory = [GRASS, STONE, SAND],
        block = STONE,
        eye = GL.Vertex3 0 0 10
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

--drawAxis = do
--    --GL.preservingMatrix $ do
--    GL.loadIdentity
--    --GL.translate $ (Vector3 0 0 0 :: Vector3 GLfloat)
--    GL.color $ GL.Color3 255 0 (0::GL.GLfloat)
--    GL.renderPrimitive Lines $
--        mapM_ (\(x,y,z) -> GL.vertex $ GL.Vertex3 x y (z::GL.GLfloat))
--            [(0,0,0),(100,0,0),(0,0,0),(0,100,0),(0,0,0),(0,0,100)]
--    --GL.color $ GL.Color4 0 0 0 (1::GL.GLfloat)

render :: State -> GLStuff -> IORef GLfloat -> IO ()
render state stuff angle = do
    let player0 = player state
        (alpha, beta) = rotation player0
        eye0 = eye player0
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    -- ModelView
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    let (x, y, z) = getSightVector (alpha, beta)
        target = (+) <$> eye0 <*> GL.Vertex3 x y z

    GLU.lookAt eye0 target (GL.Vector3 0.0 1.0 0.0)
    
    a <- get angle
    GL.rotate a $ GL.Vector3 1.0 0.0 (0.0::GL.GLfloat)
    GL.scale 0.7 0.7 (0.7::GL.GLfloat)

    GL.translate $ (Vector3 (0) (5) (0) :: Vector3 GLfloat)

    GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
    GL.preservingMatrix $ do
        cubeSide
    GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
    GL.preservingMatrix $ do
        cubeTop
    GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
    GL.preservingMatrix $ do
        cubeBot

    GLFW.swapBuffers

addMouseHandler rotation position = do
    GLFW.mousePosCallback $= \(Position x y) -> do
        let dx = x - 400
            dy = y - 300
            x' = (fromIntegral dx) * 180 / 400 / 100
            yy = (fromIntegral dy) * 90 / 300 / 100
            y' = max (-90) $ min 90 yy
        writeIORef rotation (x', (-y'))

getSightVector (alpha, beta) = 
    let x = realToFrac $ sin alpha
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
        player' = player state0
        GL.Vertex3 x y z = eye player'
        dx = mouseX - 400
        dy = mouseY - 300
        alpha = (fromIntegral dx) * 180 / 400 / 100
        yy = (fromIntegral dy) * 90 / 300 / 100
        beta = max (-90) $ min 90 yy
        rotation' = (alpha, (-beta))
        (tgX, tgY, tgZ) = getSightVector (alpha, beta)
        (crossX, crossY, crossZ) = getCrossProduct (0, 1, 0) (tgX, tgY, tgZ)

        deltaW = GL.Vertex3 (tgX*0.1) (tgY*0.1) (tgZ*0.1)
        deltaS = GL.Vertex3 (-(tgX*0.1)) (-(tgY*0.1)) (-(tgZ*0.1))
        deltaA = GL.Vertex3 (crossX*0.1) (crossY*0.1) (crossZ*0.1)
        deltaD = GL.Vertex3 (-(crossX*0.1)) (-(crossY*0.1)) (-(crossZ*0.1))
        deltaSPACE = GL.Vertex3 0 0.1 0

        stateW = processMotion state0 w deltaW
        stateS = processMotion stateW s deltaS
        stateA = processMotion stateS a deltaA
        stateD = processMotion stateA d deltaD
        stateSPACE = processMotion stateD space deltaSPACE
        state' = stateSPACE

    let eye' = (eye.player) state'
    return state { player = player' { rotation = rotation',
                                      eye = eye'
                                    }
                 }


loop :: State -> GLStuff -> Float -> IORef GLfloat -> IO ()
loop state stuff lastTime angle = do
    -- dt
    nowD <- get time
    let now = realToFrac nowD
    let dt = realToFrac $ now - lastTime

    -- game
    newState <- Main.update state dt angle
    render newState stuff angle
    angle $~! (+ 0.3)

    -- exit if window closed or Esc pressed
    esc <- GLFW.getKey GLFW.ESC
    q <- GLFW.getKey 'Q'
    open <- GLFW.getParam GLFW.Opened
    if open && esc /= GLFW.Press && q /= GLFW.Press
        then loop newState stuff now angle
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
    Position mouseX mouseY <- get GLFW.mousePos
    print (mouseX, mouseY)
    let state' = state { mouseCenter = (mouseX, mouseY) }
    stuff <- initGL
    -- setup stuff
    GLFW.swapInterval       $= 1 -- vsync
    GLFW.windowTitle        $= "Test"
    GLFW.windowSizeCallback $= resize
    --GLFW.mousePos $= Position 400 300
    GLFW.disableSpecial GLFW.MouseCursor

    -- main loop
    now <- get GLFW.time
    angle <- newIORef 0
    loop state stuff (realToFrac now) angle
    
    GLFW.closeWindow
    GLFW.terminate