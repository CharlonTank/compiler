{-# LANGUAGE OverloadedStrings #-}

module Lamdera.Compile where

import qualified System.Directory as Dir
import Make (Flags(..))
import qualified Make
import Control.Concurrent.Async

import Lamdera


-- Runs `lamdera make` of given files with no output
make :: FilePath -> FilePath -> IO ()
make root path = do
  debug $ "🏗   lamdera make " <> root <> "/" <> path

  r <- async $
    Dir.withCurrentDirectory root $
      Make.run [path] $
        Make.Flags
          { _debug = False
          , _optimize = True
          , _output = Just Make.DevNull
          , _report = Nothing
          , _docs = Nothing
          }
  wait r
  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10


-- Runs `lamdera make` with no output
make_ :: FilePath -> IO ()
make_ root = do
  debug $ "🏗   lamdera make " <> root <> "/"

  r <- async $
    Dir.withCurrentDirectory root $
      Make.run [] $
        Make.Flags
          { _debug = False
          , _optimize = True
          , _output = Just Make.DevNull
          , _report = Nothing
          , _docs = Nothing
          }
  wait r
  -- The compilation process ends by printing to terminal in a way that overwrites
  -- the progress bar – which messes with subsequent output if it gets written to
  -- stdout too quickly, as it doesn't seem to flush fast enough. Adding a small
  -- delay seems to solve the problem.
  sleep 10