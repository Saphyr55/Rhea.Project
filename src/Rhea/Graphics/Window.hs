module Rhea.Graphics.Window
  ( Window(..)
  , VideoMode(..)
  , makeWindow
  , destroy
  , swap
  , closeCallback
  , updateWindow
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad
import System.Exit ( exitSuccess )
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as S
import qualified Rhea.Input.Input as I
import Linear
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

makeWindow :: VideoMode -> IO (Maybe Window)
makeWindow mode = do
  maybeHandler <- GLFW.createWindow (width mode) (height mode) (title mode) Nothing Nothing
  GLFW.makeContextCurrent maybeHandler
  case maybeHandler of
    Just h -> do
      keyRef <- newIORef S.empty
      mouseRef <- newIORef $ MouseInfo Nothing (0, -90) (V3 0 0 (-1))
      GLFW.setCursorPosCallback h (Just $ cursorPosCallback mouseRef)
      GLFW.setKeyCallback h (Just $ callback keyRef)
      return $ Just $ Window h mode (I.Input keyRef mouseRef)
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

callback :: IORef (Set GLFW.Key) -> GLFW.KeyCallback
callback ref window key scanCode keyState modKeys = do
  -- putStrLn $ show keyState ++ " " ++ show key
  case keyState of
    GLFW.KeyState'Pressed  -> modifyIORef ref (S.insert key)
    GLFW.KeyState'Released -> modifyIORef ref (S.delete key)
    _ -> return ()
  when
    (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)