{-# LANGUAGE ScopedTypeVariables #-}

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
--import Graphics.UI.GLUT as GLUT
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
    pos :: (Int, Int, Int),
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
    player :: Player
}

makeInitState :: State
makeInitState = State {
    world = Map.fromList [],
    shown = Map.fromList [],
    sectors = fromList [],
    player = Player {
        flying = False,
        strafe = (0, 0),
        pos = (0, 0, 0),
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

render :: State -> GLStuff -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef (GLfloat, GLfloat, GLfloat) -> IO ()
render state stuff angle rotation position = do
    --drawAxes 10 10 10 10 10 10 (Colour 100) 2
    --let eye0 = eye . player $ state
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    -- ModelView
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity

    --rotation0 <-  rotation . player $ state
    (alpha, beta) <- readIORef rotation
    --print (alpha, beta)

    (x, y, z) <- readIORef position
    let eye0 = GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

    --GL.rotate (realToFrac x) $ GL.Vector3 0.0 1.0 (0.0::GL.GLfloat)
    --GL.rotate (realToFrac (-y)) $ GL.Vector3 (cos (realToFrac x)) (0.0::GL.GLfloat) (sin (realToFrac x))
    let (x, y, z) = getSightVector (alpha, beta)
        --x = realToFrac (sin alpha)
        --y = realToFrac $ sin beta
        --z = realToFrac $ (cos beta) * (-(cos alpha))
        target = (+) <$> eye0 <*> GL.Vertex3 x y z
        --GL.Vertex3 a b c = eye0
        --target = GL.Vertex3 (x+a) (y+b) (z+c)

    --GLU.lookAt eye0 (GL.Vertex3 0.0 0.0 0.0) (GL.Vector3 0.0 1.0 0.0)
    GLU.lookAt eye0 target (GL.Vector3 0.0 1.0 0.0)

    -- level
    --GL.textureBinding GL.Texture2D $= Just (texFloor stuff)
    --renderLevel $ level state

    -- cube
    
    a <- get angle
    GL.rotate a $ GL.Vector3 1.0 0.0 (0.0::GL.GLfloat)
    GL.scale 0.7 0.7 (0.7::GL.GLfloat)

    GL.translate $ (Vector3 (0) (5) (0) :: Vector3 GLfloat)

    GL.textureBinding GL.Texture2D $= Just (grass_sid stuff)
    GL.preservingMatrix $ do
        --GL.translate $ (Vector3 0 0 0 :: Vector3 GLfloat)
        cubeSide
    GL.textureBinding GL.Texture2D $= Just (grass_bot stuff)
    GL.preservingMatrix $ do
        --GL.translate $ (Vector3 0 0 0 :: Vector3 GLfloat)
        cubeTop
    GL.textureBinding GL.Texture2D $= Just (grass_top stuff)
    GL.preservingMatrix $ do
        --GL.translate $ (Vector3 0 0 0 :: Vector3 GLfloat)
        cubeBot


    --GL.preservingMatrix $ do
    --    GL.scale 10 10 (10::GL.GLfloat)
    --    GL.color (Color3 100 100 (10::GL.GLfloat))
    --    GL.rasterPos (Vertex2 50 (50::GL.GLfloat))
    --    GLFW.renderString GLFW.Fixed8x16 "text"

    --GL.matrixMode $= GL.Modelview 0
    GLFW.swapBuffers

addMouseHandler rotation position = do
    GLFW.mousePosCallback $= \(Position x y) -> do
        let dx = x - 400
            dy = y - 300
            x' = (fromIntegral dx) * 180 / 400 / 100
            yy = (fromIntegral dy) * 90 / 300 / 100
            y' = max (-90) $ min 90 yy
        writeIORef rotation (x', (-y'))
        --(xxx,yyy) <- readIORef rotation
        --GLFW.mousePos $= Position 400 300
        --putStrLn $ "callback position: " ++ show (x,y)
        --putStrLn $ "rotation: " ++ show (xxx,yyy)
        --return ()

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

checkWASD rotation position = do
    w <- GLFW.getKey 'W'
    a <- GLFW.getKey 'A'
    s <- GLFW.getKey 'S'
    d <- GLFW.getKey 'D'
    space <- GLFW.getKey ' '

    when (w == GLFW.Press) $ do
        (alpha, beta) <- readIORef rotation
        (x, y, z) <- readIORef position
        let (tgX, tgY, tgZ) = getSightVector (alpha, beta)
        writeIORef position (x+tgX*0.1, y+tgY*0.1, z+tgZ*0.1)
        --return ()
    when (s == GLFW.Press) $ do
        (alpha, beta) <- readIORef rotation
        (x, y, z) <- readIORef position
        let (tgX, tgY, tgZ) = getSightVector (alpha, beta)
        writeIORef position (x-tgX*0.1, y-tgY*0.1, z-tgZ*0.1)
        --return ()
    when (a == GLFW.Press) $ do
        (alpha, beta) <- readIORef rotation
        (x, y, z) <- readIORef position
        let sightVector = getSightVector (alpha, beta)
            (tgX, tgY, tgZ) = getCrossProduct (0, 1, 0) sightVector
        writeIORef position (x+tgX*0.1, y+tgY*0.1, z+tgZ*0.1)
        --return ()
    when (d == GLFW.Press) $ do
        (alpha, beta) <- readIORef rotation
        (x, y, z) <- readIORef position
        let sightVector = getSightVector (alpha, beta)
            (tgX, tgY, tgZ) = getCrossProduct (0, 1, 0) sightVector
        writeIORef position (x-tgX*0.1, y-tgY*0.1, z-tgZ*0.1)
        --return ()
    when (space == GLFW.Press) $ do
        (alpha, beta) <- readIORef rotation
        (x, y, z) <- readIORef position
        writeIORef position (x, y+1*0.1, z)
        --return ()

    --return ()

update state dt angle = do
    mousePosition <- get GLFW.mousePos
    let (mouseX, mouseY) = Position mousePosition
        player' = player state
        dx = mouseX - 400
        dy = mouseY - 300
        mouseX' = (fromIntegral dx) * 180 / 400 / 100
        yy = (fromIntegral dy) * 90 / 300 / 100
        mouseY' = max (-90) $ min 90 yy
        rotation' = (mouseX', (-mouseY'))

    return state { player = player' { rotation = rotation'

                                    }
                 }


--loop :: State -> GLStuff -> Float -> IORef GLfloat -> IO ()
loop state stuff lastTime angle = do
    -- dt
    nowD <- get time
    let now = realToFrac nowD
    let dt = realToFrac $ now - lastTime

    -- game
    newState <- Main.update state dt angle
    render newState stuff angle
    --render state stuff angle rotation position
    angle $~! (+ 0.3)

    --checkWASD rotation position

    -- exit if window closed or Esc pressed

    --mouse <- get GLFW.mousePos
    --print mouse

    esc <- GLFW.getKey GLFW.ESC
    q <- GLFW.getKey 'Q'
    open <- GLFW.getParam GLFW.Opened
    if open && esc /= GLFW.Press && q /= GLFW.Press
        --then loop newState stuff now
        then loop state stuff now angle rotation position
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
    stuff <- initGL
    -- setup stuff
    GLFW.swapInterval       $= 1 -- vsync
    GLFW.windowTitle        $= "Test"
    GLFW.windowSizeCallback $= resize
    GLFW.mousePos $= Position 400 300
    GLFW.disableSpecial GLFW.MouseCursor

    -- main loop
    now <- get GLFW.time
    angle <- newIORef 0
    rotation <- newIORef (0, 0)
    position <- newIORef (0, 0, 10)
    --addMouseHandler rotation position
    loop state stuff (realToFrac now) angle
    -- position exit
    GLFW.closeWindow
    GLFW.terminate