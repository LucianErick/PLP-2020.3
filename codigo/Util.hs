module Util where


removeJustMaybeList :: Maybe[t] -> [t]
removeJustMaybeList (Just a) = a

removeJustListOfMaybe :: [Maybe t] -> [t]
removeJustListOfMaybe [] = []
removeJustListOfMaybe (Just x: xs) = [x] ++ removeJustListOfMaybe xs

