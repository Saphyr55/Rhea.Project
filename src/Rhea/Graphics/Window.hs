module Rhea.Graphics.Window
  ( Window(..)
  , VideoMode(..)
  , createWindow
  , destroy
  , swap
  , closeCallback
  , updateWindow
  , enableInputMode
  , disableInputMode
  ) where


import Rhea.Input.Mouse
import qualified Rhea.Input.Input as I
import qualified Graphics.UI.GLFW as GLFW
import System.Exit ( exitSuccess )
import Data.IORef
import qualified Data.Set as S
import Linear

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
      buttonPRef <- newIORef S.empty
      buttonRRef <- newIORef S.empty
      mouseRef <- newIORef
        $ MouseInfo Nothing (0, -90) (V3 0 0 (-1))

      GLFW.setKeyCallback h
        $ Just
        $ I.callback keyPRef keyRRef

      GLFW.setCursorPosCallback h 
        $ Just 
        $ mouseCallback mouseRef

      return
        $ Just
        $ Window h mode
        $ I.Input keyPRef keyRRef buttonPRef buttonRRef mouseRef

    Nothing -> return Nothing

destroy :: Window -> IO ()
destroy win = do
  GLFW.destroyWindow (handler win)

swap :: Window -> IO ()
swap window =
  GLFW.swapBuffers $ handler window

updateWindow :: Window -> IO Window
updateWindow window = (\(w, h) -> 
    window { 
      videoMode = (videoMode window)
        { width = w
        , height = h } })
  <$> GLFW.getWindowSize (handler window)
 
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

enableInputMode :: Window -> IO ()
enableInputMode w =
  GLFW.setCursorInputMode (handler w)  GLFW.CursorInputMode'Normal

disableInputMode :: Window -> IO ()
disableInputMode w =
  GLFW.setCursorInputMode (handler w) GLFW.CursorInputMode'Disabled
