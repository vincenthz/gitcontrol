module System.GitControl.Class
    where

import Data.ByteString (ByteString)
import System.GitControl.Types (AccessMode)

class GitControl c where
    hasRight :: c -> ByteString -> ByteString -> AccessMode -> IO Bool
