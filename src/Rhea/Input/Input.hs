module Rhea.Input.Input
  ( Input(..)
  , callback
  , isPressed'Key
  , onPressed'Key
  ) where

import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Set as S
import Data.Set (Set)
import Data.IORef
import Control.Monad
import Rhea.Input.Mouse (MouseInfo)

data Input = Input
  { keysPressed    :: IORef (Set GLFW.Key)
  , keysReleased   :: IORef (Set GLFW.Key)
  , mousesPressed  :: IORef (Set GLFW.MouseButton)
  , mousesReleased :: IORef (Set GLFW.MouseButton)
  , mouseInfo      :: IORef MouseInfo
  }

isPressed'Key :: Input -> GLFW.Key -> IO Bool
isPressed'Key input key =
  S.member key <$> readIORef (keysPressed input)

onPressed'Key :: Input -> (GLFW.Key -> b -> b) -> b -> IO b
onPressed'Key input f b =
  S.foldr f b <$> readIORef (keysPressed input)

callback :: IORef (Set GLFW.Key) -> IORef (Set GLFW.Key) -> GLFW.KeyCallback
callback keysPRef keysRRef window key _ keyState _ = do
  case keyState of
    GLFW.KeyState'Pressed -> do
      modifyIORef keysPRef (S.insert key)
      modifyIORef keysRRef (S.delete key)
    GLFW.KeyState'Released -> do
      modifyIORef keysPRef (S.delete key)
      modifyIORef keysRRef (S.insert key)
    _ -> return ()
  when
    (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
    (GLFW.setWindowShouldClose window True)