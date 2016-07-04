{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import System.Environment (getArgs)

import Match


-- Constants
idMouse :: Text
idMouse = "    <owl:Class rdf:about=\"http://mouse.owl#"
idHuman :: Text
idHuman = "    <owl:Class rdf:about=\"http://human.owl#"
labelAny :: Text
labelAny = "        <rdfs:label rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">"


main :: IO ()
main = do
    args <- getArgs
    case args of
        [owlFile, humanFile] -> run owlFile humanFile
        _ -> putStrLn "match <path to mouse.owl> <path to human.owl>"


run :: String -> String -> IO ()
run mouseFile humanFile = printMatches =<< findMatches <$> mouseLabels <*> humanLabels
  where
    mouseLabels = parseFile idMouse labelAny mouseFile
    humanLabels = parseFile idHuman labelAny humanFile


parseFile :: Text -> Text -> String -> IO (Map Text Text)
parseFile idPre labelPre file = M.fromList . pairUp . ids <$> input
  where
    input = T.lines . decodeUtf8 <$> BS.readFile file
    ids = consumer idPre (T.take 10) labels
    labels = consumer labelPre (T.takeWhile (/= '<')) ids


consumer :: Text -> (Text -> t) -> ([Text] -> [t]) -> [Text] -> [t]
consumer _ _ _ [] = []
consumer prefix taker next (l:ls) = case T.stripPrefix prefix l of
    Just suffix -> taker suffix : next ls
    Nothing -> consumer prefix taker next ls


pairUp :: [Text] -> [(Text, Text)]
pairUp [] = []
pairUp [_] = []
pairUp (c : l : xs) = (replace $ T.toLower l, c) : pairUp xs
  where
    replace = compose $ map (`T.replace` " ") ["-", " or ", " and ", "/", "_"]
    compose = foldr (.) id


printMatches :: [(Text, Text)] -> IO ()
printMatches = mapM_ (\(m, h) -> putStrLn $ T.unpack m ++ ", " ++ T.unpack h)
