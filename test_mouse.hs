import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)

data Action = Action (IO Action)

main = do
  -- invoke either active or passive drawing loop depending on command line argument
  args <- getArgs
  prog <- getProgName
  case args of
    ["active"]  -> putStrLn "Running in active mode" >> main' active
    ["passive"] -> putStrLn "Running in passive mode" >> main' passive
    _ -> putStrLn $ "USAGE: " ++ prog ++ " [active|passive]"
 
main' run = do
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 400 400) [GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= "GLFW Demo"
  GL.shadeModel    $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend      $= GL.Enabled
  GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth  $= 1.5
  -- set the color to clear background
  GL.clearColor $= Color4 0 0 0 0
 
  -- set 2D orthogonal view inside windowSizeCallback because
  -- any change to the Window size should result in different
  -- OpenGL Viewport.
  GLFW.windowSizeCallback $= \ size@(GL.Size w h) ->
    do
      GL.viewport   $= (GL.Position 0 0, size)
      GL.matrixMode $= GL.Projection
      GL.loadIdentity
      GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
  -- keep all line strokes as a list of points in an IORef
  lines <- newIORef []
  -- run the main loop
  run lines
  -- finish up
  GLFW.closeWindow
  GLFW.terminate

 -- we start with waitForPress action
active lines = loop waitForPress
  where 
 
    loop action = do
      -- draw the entire screen
      render lines
      -- swap buffer
      GLFW.swapBuffers
      -- check whether ESC is pressed for termination
      p <- GLFW.getKey GLFW.ESC
      unless (p == GLFW.Press) $
        do
            -- perform action
            Action action' <- action
            -- sleep for 1ms to yield CPU to other applications
            GLFW.sleep 0.001
 
            -- only continue when the window is not closed
            windowOpen <- getParam Opened
            unless (not windowOpen) $
              loop action' -- loop with next action
 
    waitForPress = do
      b <- GLFW.getMouseButton GLFW.ButtonLeft
      case b of
        GLFW.Release -> return (Action waitForPress)
        GLFW.Press   -> do
          -- when left mouse button is pressed, add the point
          -- to lines and switch to waitForRelease action.
          (GL.Position x y) <- GL.get GLFW.mousePos 
          modifyIORef lines (((x,y):) . ((x,y):))
          return (Action waitForRelease)
 
    waitForRelease = do
        -- keep track of mouse movement while waiting for button 
        -- release
        (GL.Position x y) <- GL.get GLFW.mousePos
        -- update the line with new ending position
        modifyIORef lines (((x,y):) . tail)
        b <- GLFW.getMouseButton GLFW.ButtonLeft
        case b of
          -- when button is released, switch back back to 
          -- waitForPress action
          GLFW.Release -> return (Action waitForPress)
          GLFW.Press   -> return (Action waitForRelease)

passive lines = do
  -- disable auto polling in swapBuffers
  GLFW.disableSpecial GLFW.AutoPollEvent
 
  -- keep track of whether ESC has been pressed
  quit <- newIORef False
 
  -- keep track of whether screen needs to be redrawn
  dirty <- newIORef True
 
  -- mark screen dirty in refresh callback which is often called
  -- when screen or part of screen comes into visibility.
  GLFW.windowRefreshCallback $= writeIORef dirty True
 
  -- use key callback to track whether ESC is pressed
  GLFW.keyCallback $= \k s -> 
     when (fromEnum k == fromEnum GLFW.ESC && s == GLFW.Press) $ 
        writeIORef quit True
 
  -- Terminate the program if the window is closed
  GLFW.windowCloseCallback $= (writeIORef quit True >> return True)
 
  -- by default start with waitForPress
  waitForPress dirty
  loop dirty quit
  where
 
    loop dirty quit = do
        GLFW.waitEvents
        -- redraw screen if dirty
        d <- readIORef dirty
 
        when d $ 
          render lines >> GLFW.swapBuffers
 
        writeIORef dirty False
        -- check if we need to quit the loop
        q <- readIORef quit
        unless q $
          loop dirty quit
 
    waitForPress dirty =
      do
        --GLFW.mousePosCallback    $= \_ -> return ()
        GLFW.mousePosCallback   $= \s ->
            when (True) $
              do
                -- when left mouse button is pressed, add the point
                -- to lines and switch to waitForRelease action.
                (GL.Position x y) <- GL.get GLFW.mousePos
                modifyIORef lines (((x,y):) . ((x,y):))
                waitForRelease dirty
 
        GLFW.mouseButtonCallback $= \b s -> 
            when (b == GLFW.ButtonLeft && s == GLFW.Press) $
              do
                -- when left mouse button is pressed, add the point
                -- to lines and switch to waitForRelease action.
                (GL.Position x y) <- GL.get GLFW.mousePos
                modifyIORef lines (((x,y):) . ((x,y):))
                waitForRelease dirty
 
    waitForRelease dirty = 
      do 
        GLFW.mousePosCallback $= \(Position x y) ->
          do
            -- update the line with new ending position
            modifyIORef lines (((x,y):) . tail)
            -- mark screen dirty
            writeIORef dirty True
 
        GLFW.mouseButtonCallback $= \b s ->
            -- when left mouse button is released, switch back to
            -- waitForPress action.
            when (b == GLFW.ButtonLeft && s == GLFW.Release) $
              waitForPress dirty

render lines = do
  l <- readIORef lines
  GL.clear [GL.ColorBuffer]
  GL.color $ color3 1 0 0
  GL.renderPrimitive GL.Lines $ mapM_
      (\ (x, y) -> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0)) l
 
 
vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3
 
 
color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3