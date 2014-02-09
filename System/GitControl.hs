-- |
-- Module      : System.GitControl
-- License     : BSD-style
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE OverloadedStrings #-}
module System.GitControl
    ( defaultMain
    , GitControl(..)
    , module System.GitControl.Types
    ) where

import System.Posix.Env.ByteString
import System.Posix.Process.ByteString
import System.Exit

import System.GitControl.Shell
import System.GitControl.Class
import System.GitControl.Types

defaultMain :: GitControl a => (IO a) -> IO ()
defaultMain dbGet = do
    args <- getArgs
    case args of
        [user] -> authUser user
        _      -> error "invalid command line"
  where authUser userName = do
            db   <- dbGet
            envs <- getEnvironment
            case lookup "SSH_ORIGINAL_COMMAND" envs of
                Nothing   -> putStrLn "error, command not found"
                Just ocmd -> do let theCmd = commandLineParser ocmd
                                authorized <- isAuthorized db
                                                           (Username userName)
                                                           (RepositoryPath $ head $ gitCmdArgs theCmd)
                                                           (accessMode theCmd)
                                -- TODO sanitize command, args..
                                if authorized
                                    then executeFile (gitCmd theCmd) True (gitCmdArgs theCmd) (Just envs)
                                    else exitFailure
