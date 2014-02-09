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

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Byteable

defaultMain :: GitControl a
            => ByteString -- ^ repository root directory, automatically appended to the queried repository
            -> IO a       -- ^ initialize a git control backend
            -> IO ()
defaultMain repoRoot getBackend
    | B.length repoRoot == 0  = error "invalid repoRoot"
    | B.last repoRoot /= 0x2f = error "repoRoot need to terminate by /"
    | otherwise = do
        args <- getArgs
        case args of
            [user] -> authUser $ Username user
            _      -> error "invalid command line"
  where authUser userName = do
            envs <- getEnvironment
            case lookup "SSH_ORIGINAL_COMMAND" envs of
                Nothing   -> error "SSH_ORIGINAL_COMMAND not found"
                Just ocmd -> either doFailure (doCheck envs userName) $ commandParse ocmd
        doFailure _ = exitFailure
        doCheck envs user cmd = do
            backend    <- getBackend
            authorized <- isAuthorized backend user (getCommandRepository cmd) (commandToAccess cmd)
            -- TODO sanitize envs ?
            if authorized
                then executeFile (commandToRaw cmd) True [repoRoot `B.append` (toBytes $ getCommandRepository cmd)] (Just envs)
                else exitFailure
