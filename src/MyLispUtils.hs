module MyLispUtils where

import Control.Monad.Error
import Foreign

unwordsShow :: (Show a) => ([a] -> String)
unwordsShow = unwords . map show

unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:[]) = x
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

foldrBool :: (a -> a -> Bool) -> [a] -> Bool
foldrBool f [] = True
foldrBool f [_] = True
foldrBool f (x:xs) = (x `f` head xs) && (foldrBool f xs)

finallyError :: MonadError e m => m a -> m b -> m a
finallyError tryBlock finallyBlock =
    catchError (do ret <- tryBlock
                   finallyBlock
                   return ret)
               (\e -> finallyBlock >> throwError e)

equalByPtr a b = do
    pa <- newStablePtr a
    pb <- newStablePtr b
    let res = pa == pb
    freeStablePtr pa
    freeStablePtr pb
    return res