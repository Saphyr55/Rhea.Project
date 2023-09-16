module Core.Closeable 
  ( Closeable(..)
  ) where

class Closeable t where

    close :: t -> IO ()
    