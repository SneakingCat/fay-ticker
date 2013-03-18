module Main (main) where

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo

import System.Process (runCommand, waitForProcess)
import System.Exit (exitWith)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  {postBuild = buildFay
  }
  
-- | Build the client.
buildFay :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
buildFay _ _ pkgdesc buildinfo = do
  putStrLn "Building the client in a shell command ..."
  pid <- runCommand "fay --include ./src --Wall examples/TickerApp.hs"
  code <- waitForProcess pid
  exitWith code