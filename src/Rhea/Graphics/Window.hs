module Rhea.Graphics.Window
  (
    Window(..),
    VideoMode(..),
    makeWindow,
    destroy,
    swap,
    closeCallback,
    updateWindow
  ) where

import qualified Graphics.UI.GLFW as GLFW
import System.Exit ( exitSuccess )

data VideoMode = VideoMode
  { width  :: Int
  , height :: Int
  , title  :: String 
  } deriving ( Show )

data Window = Window
  { handler   :: GLFW.Window
  , videoMode :: VideoMode 
  }

makeWindow :: VideoMode -> IO (Maybe Window)
makeWindow mode = do
  maybeHandler <- GLFW.createWindow (width mode) (height mode) (title mode) Nothing Nothing
  GLFW.makeContextCurrent maybeHandler
  case maybeHandler of
    Just h -> return $ Just $ Window h mode
    Nothing -> return Nothing

destroy :: Window -> IO ()
destroy win = do
  GLFW.destroyWindow (handler win)

swap :: Window -> IO ()
swap window =
  GLFW.swapBuffers $ handler window

updateWindow :: Window -> IO Window
updateWindow window = do
  (w, h) <- GLFW.getWindowSize $ handler window
  return
    $ Window (handler window)
    $ VideoMode w h
    $ title
    $ videoMode window

closeCallback :: Window -> IO ()
closeCallback window =
  GLFW.setWindowCloseCallback
    (handler window)
    (Just
        ( \win -> do
            GLFW.destroyWindow win
            GLFW.terminate
            _ <- exitSuccess
            return ()
        )
    )
