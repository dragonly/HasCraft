module Etc where

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.UI.GLFW as GLFW

v x y z = GL.vertex (GL.Vertex3 x y z :: GL.Vertex3 GLfloat)
n x y z = GL.normal (GL.Normal3 x y z :: GL.Normal3 GLfloat)
t u v   = GL.texCoord (GL.TexCoord2 u v :: GL.TexCoord2 GLfloat)

drawCubeSide :: IO ()
drawCubeSide = renderPrimitive Quads $ do
    -- back
    n 0 0 (-1)
    t 0 1 >> v (-1) (-1) (-1)
    t 1 1 >> v   1  (-1) (-1)
    t 1 0 >> v   1    1  (-1)
    t 0 0 >> v (-1)   1  (-1)
    -- front
    n 0 0 1
    t 0 1 >> v   1  (-1)   1
    t 1 1 >> v (-1) (-1)   1
    t 1 0 >> v (-1)   1    1
    t 0 0 >> v   1    1    1
    -- left
    n (-1) 0 0
    t 0 1 >> v (-1) (-1)   1
    t 1 1 >> v (-1) (-1) (-1)
    t 1 0 >> v (-1)   1  (-1)
    t 0 0 >> v (-1)   1    1
    -- right
    n 1 0 0
    t 0 1 >> v   1  (-1) (-1)
    t 1 1 >> v   1  (-1)   1
    t 1 0 >> v   1    1    1
    t 0 0 >> v   1    1  (-1)
drawCubeTop :: IO ()
drawCubeTop = renderPrimitive Quads $ do
    -- top
    n 0 1 0
    t 0 1 >> v   1    1  (-1)
    t 1 1 >> v   1    1    1
    t 1 0 >> v (-1)   1    1
    t 0 0 >> v (-1)   1  (-1)
drawCubeBot :: IO ()
drawCubeBot = renderPrimitive Quads $ do
    -- bottom
    n 0 (-1) 0
    t 0 1 >> v   1  (-1)   1
    t 1 1 >> v   1  (-1) (-1)
    t 1 0 >> v (-1) (-1) (-1)
    t 0 0 >> v (-1) (-1)   1


loadTexture :: String -> IO GL.TextureObject
loadTexture filename = do
    dataFileName <- return filename
    [texName] <- GL.genObjectNames 1
    GL.textureBinding Texture2D $= Just texName
    GLFW.loadTexture2D dataFileName [GLFW.BuildMipMaps]
    GL.textureFilter Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear') -- trilinear filtering
    return texName