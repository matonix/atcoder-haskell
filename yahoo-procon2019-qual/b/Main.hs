module Main where

import Control.Monad
import Data.List
import Debug.Trace

main :: IO ()
main = do
  paths' <- map getPair <$> replicateM 3 getLine
  putStrLn $ if p paths' then "YES" else "NO"
  where
    p :: [(Int, Int)] -> Bool
    p paths'' = any (hasPath [] paths'') [0..3]
    hasPath acc paths node
      | length acc == 3 = True
      | null paths = False
      | otherwise = case getNext node paths of
        Just (nextNode, nextPath) ->
          hasPath (nextNode : acc) (delete nextPath paths) nextNode
        Nothing -> False
    getNext n ps =
      case find (\(x, y) -> n == x || n == y) ps of
        Nothing -> Nothing
        Just nP -> let
          nN = if fst nP == n then snd nP else fst nP
          in Just (nN, nP)
 

getPair = (\[x, y] -> (x, y)) 
  . map pred
  . map (read::String -> Int) 
  . words