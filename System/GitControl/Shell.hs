{-# LANGUAGE OverloadedStrings #-}
module System.GitControl.Shell
    ( -- * Command line parser
      GitCommand(..)
    , commandLineParser
    ) where

import System.GitControl.Types
import qualified Data.ByteString.Char8   as BS
import qualified Data.List               as L

data GitCommand = GitCommand
    { gitCmd     :: BS.ByteString
    , gitCmdArgs :: [BS.ByteString]
    , accessMode :: AccessMode
    } deriving (Eq, Show)

commandLineParser :: BS.ByteString -> GitCommand
commandLineParser cl =
    let list = L.filter ((/=) BS.empty) $ BS.split ' ' cl
    in  case list of
            (cmd:xs) -> GitCommand cmd (L.map removeUselessQuotes xs) (commandLineType cmd)
            []       -> GitCommand BS.empty [] None
    where
        removeUselessQuotes :: BS.ByteString -> BS.ByteString
        removeUselessQuotes s =
            let s1 = if ((BS.head s) == '\'') then BS.drop 1 s else s
            in if ((BS.last s1) == '\'') then BS.take ((BS.length s1) - 1) s1 else s1

commandLineType :: BS.ByteString -> AccessMode
commandLineType cmd
    | cmd == "git-receive-pack"   = AccessRead
    | cmd == "git-upload-pack"    = AccessWrite
    | cmd == "git-upload-archive" = AccessWrite
    | cmd == "git-shell"          = None -- Need more investigation
    | otherwise = None
