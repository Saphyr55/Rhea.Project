module Rhea.Graphics.Window
  ( Window(..)
  , VideoMode(..)
  , createWindow
  , destroy
  , swap
  , closeCallback
  , updateWindow
  ) where

import qualified Graphics.UI.GLFW as GLFW
import System.Exit ( exitSuccess )
import Data.IORef
import qualified Data.Set as S
import qualified Rhea.Input.Input as I
import Linear
import Rhea.Input.Input
import Rhea.Input.Mouse

data VideoMode = VideoMode
  { width  :: Int
  , height :: Int
  , title  :: String 
  } deriving ( Show )

data Window = Window
  { handler   :: GLFW.Window
  , videoMode :: VideoMode
  , input     :: I.Input
  }

createWindow :: VideoMode -> IO (Maybe Window)
createWindow mode = do
  maybeHandler <- GLFW.createWindow (width mode) (height mode) (title mode) Nothing Nothing
  GLFW.makeContextCurrent maybeHandler
  case maybeHandler of
    Just h -> do
      keyPRef <- newIORef S.empty
      keyRRef <- newIORef S.empty
      mouseRef <- newIORef $ MouseInfo Nothing (0, -90) (V3 0 0 (-1))
      GLFW.setKeyCallback h $ Just $ callback keyPRef keyRRef
      return $ Just $ Window h mode $ I.Input keyPRef keyRRef
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
    ( VideoMode w h
    $ title
    $ videoMode window )
    (input window)

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

