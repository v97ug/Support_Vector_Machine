module CalTFIDF where

import Type
import qualified Data.Map as Map
import Data.Maybe
import Control.Arrow
import Data.List

calTFIDF :: Int -> Int -> Int -> Double
calTFIDF n tf df =
  let n' = fromIntegral n
      tf' = fromIntegral tf
      df' = fromIntegral df
  in tf' * log (n' / df')

eachDocument :: Int -> Map.Map String Int -> [(String, Int)] -> Vector
eachDocument n tf = map (eachWord n tf)

eachWord :: Int -> Map.Map String Int -> (String, Int) -> Double
eachWord n tf (dfWord, dfCount) =
  let tfValue = Map.lookup dfWord tf in
  if isNothing tfValue then 0 else calTFIDF n (fromJust tfValue) dfCount

calTF :: [[String]] -> [Map.Map String Int]
calTF = map (Map.fromList . toWordCountTuple)

-- example. ["aaa","bbb","aaa"] -> [("aaa",2),("bbb",1)]
toWordCountTuple :: [String] -> [(String, Int)]
toWordCountTuple = map (head &&& length) . group . sort

calDF :: [Map.Map String Int] -> [(String, Int)]
calDF tf =
  let words' = concatMap Map.keys tf :: [String]
  in toWordCountTuple words'
