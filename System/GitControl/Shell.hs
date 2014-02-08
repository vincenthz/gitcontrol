module System.GitControl.Shell
    ( -- * Command line parser
      GitCommand(..)
    , commandLineParser
    ) where

import qualified Data.ByteString.Char8   as BS
import qualified Data.List               as L

data GitCommand = GitCommand
    { gitCmd     :: BS.ByteString
    , gitCmdArgs :: [BS.ByteString]
    } deriving (Eq, Show)

commandLineParser :: BS.ByteString -> GitCommand
commandLineParser cl =
    let list = L.filter ((/=) BS.empty) $ BS.split ' ' cl
    in  GitCommand (L.head list) (L.map removeUselessQuotes $ L.tail list)
    where
        removeUselessQuotes :: BS.ByteString -> BS.ByteString
        removeUselessQuotes s =
            let s1 = if ((BS.head s) == '\'') then BS.drop 1 s else s
            in if ((BS.last s1) == '\'') then BS.take ((BS.length s1) - 1) s1 else s1
