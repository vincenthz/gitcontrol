{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module System.GitControl.Class
    where

class Eq v => GitControl c v |c -> v where
    init      :: IO c
    save      :: c -> IO ()
    insert    :: v -> c -> Either c c
    member    :: (v -> Bool) -> c -> Maybe v
    getBy     :: Int -> c -> Maybe v
