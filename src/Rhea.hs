module Rhea
    ( run
    ) where

import qualified Core.Application as App
import qualified Graphics.Window as Window (Window, VideoMode, create)

data Context = Context
  { window :: Window.Window }

videoMode :: Window.VideoMode
videoMode =
  Window.VideoMode 1080 720 "Demo"

context :: IO (Maybe Context)
context = do
  m <- Window.create videoMode
  case m of
    Just window -> return $ Just $ Context window
    Nothing -> return Nothing

run :: IO ()
run = do
  context <- context
  maybe () App.run context return