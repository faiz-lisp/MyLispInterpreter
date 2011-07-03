module MyLispUtils where

unwordsShow :: (Show a) => ([a] -> String)
unwordsShow = unwords . (map show)
