module System.GitControl.Class
    where

import System.GitControl.Types

class GitControl c where
    isAuthorized :: c -> Username -> RepositoryPath -> AccessMode -> IO Bool
