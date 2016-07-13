{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import System.Environment (getArgs)

import Match


main :: IO ()
main = do
    args <- getArgs
    case args of
        cutoff : _ -> run (read cutoff) "mouse_full" "human_full"
        _ -> putStrLn "match <path to mouse.owl> <path to human.owl>"


run :: Double -> String -> String -> IO ()
run cutoff mouseFile humanFile = printMatches =<< findMatches cutoff <$> mouseAnnos <*> humanAnnos
  where
    mouseAnnos = parseFile mouseFile
    humanAnnos = parseFile humanFile


parseFile :: String -> IO (Map Text (Text, [Text]))
parseFile inFile = M.fromList . mapMaybe parseLine . T.lines . decodeUtf8 <$> BS.readFile inFile
  where
    parseLine l = case T.splitOn "|" l of
        key : labels -> Just (label, (key, syns))
          where
            label : syns = nub labels
        _ -> Nothing


printMatches :: [(Text, Text)] -> IO ()
printMatches = mapM_ (\(m, h) -> putStrLn $ T.unpack m ++ ", " ++ T.unpack h)
