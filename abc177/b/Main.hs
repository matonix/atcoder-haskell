module Main where
import qualified Data.ByteString.Char8         as BS

main :: IO ()
main = do
  s <- BS.getLine
  t <- BS.getLine
  let tl = BS.length t
  -- print [ (take tl $ drop i s) | i <- [0..length s-tl] ]
  print $ minimum [ differences (BS.take tl $ BS.drop i s) t | i <- [0..BS.length s-tl] ]

differences a b = length $ filter id $ BS.zipWith (/=) a b
