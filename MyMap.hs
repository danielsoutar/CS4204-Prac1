module MyMap where

map' :: (a -> b) -> [a] -> [b]
map' func [] = []
map' func (x:xs) = func x : map func xs