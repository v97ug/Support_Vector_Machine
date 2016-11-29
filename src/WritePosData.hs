import System.IO
import System.Environment
import qualified Data.Map as Map

import CalTFIDF
import Type

eachVector :: [(Int, Double)] -> String
eachVector tupleArr = "+1 " ++ unwords (map (\x -> show (fst x) ++ ":" ++ show (snd x)) tupleArr)

main :: IO ()
main = do
  fileNames <- getArgs
  posStr <- readFile (head fileNames)
  let documents = lines posStr
      n = length documents
      splitWords = map words documents :: [[String]]
      tf = calTF splitWords :: [Map.Map String Int]
      df = calDF tf :: [(String, Int)]
      featureVectors = map (\oneTF -> eachDocument n oneTF df) tf :: [Vector]
      indexVectors = map (zip [1..]) featureVectors :: [[(Int, Double)]]
      not0Vectors = map (filter (\x -> snd x > 0)) indexVectors :: [[(Int, Double)]]
      resultStr = unlines $ map eachVector not0Vectors
  withFile "posLearn.txt" WriteMode $ \handle -> hPutStr handle resultStr
