module Graphics.Window 
  (
    Window,
    VideoMode,
    create,
    destroy,
  ) where

import qualified Graphics.UI.GLFW as GLFW (Window, createWindow, destroyWindow)

data VideoMode = VideoMode
    { width  :: Int,
      height :: Int,
      title  :: String }
    deriving ( Show )

data Window = Window 
    { handler   :: GLFW.Window,
      mode :: VideoMode }

create :: VideoMode -> IO (Maybe Window)
create mode = do
  maybeHandler <- GLFW.createWindow (width mode) (height mode) (title mode) Nothing Nothing
  case maybeHandler of
    Just h -> return $ Just $ Window h mode
    Nothing -> return Nothing


destroy :: Window -> IO ()
destroy win = do 
    GLFW.destroyWindow (handler win)


    