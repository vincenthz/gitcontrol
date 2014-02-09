module System.GitControl.Class
    where

import System.GitControl.Types

class GitControl c where
    hasRight :: c -> Username -> RepositoryPath -> AccessMode -> IO Bool
