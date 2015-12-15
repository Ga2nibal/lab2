module Modules.Helpers where

import qualified Data.Map as M

type ClassDataMap = M.Map Int [Double]
type TrainedData  = M.Map String ClassDataMap
type DataVector   = ([Double], String)
type PrintSource = Maybe String

mapWithIndex :: ((Int , a) -> b) -> [a] -> [b]
mapWithIndex f xs = map f (zip [0..] xs)

-----------------------------Input-----------------------
safetail :: [a] -> [a]
safetail [] = []
safetail xs = tail xs

safeinit :: [a] -> [a]
safeinit [] = []
safeinit xs = init xs

listTransform :: Bool -> Bool -> [a] -> [a]
listTransform ignFirst ignLast
    | ignFirst  = listTransform False ignLast  . safetail
    | ignLast   = listTransform ignFirst False . safeinit
    | otherwise = id
