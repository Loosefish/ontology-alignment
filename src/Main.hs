{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs)

import Match


-- Constants
idMouse :: Text
idMouse = "    <owl:Class rdf:about=\"http://mouse.owl#"
idHuman :: Text
idHuman = "    <owl:Class rdf:about=\"http://human.owl#"
labelAny :: Text
labelAny = "        <rdfs:label rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">"
quality :: Float
quality = 0.545


main :: IO ()
main = do
    args <- getArgs
    case args of
        [owlFile, humanFile] -> run owlFile humanFile
        _ -> putStrLn "match <path to mouse.owl> <path to human.owl>"


run :: String -> String -> IO ()
run mouseFile humanFile = do
    -- Read input files
    mouseLabels <- parse idMouse labelAny <$> readText mouseFile
    humanLabels <- parse idHuman labelAny <$> readText humanFile
    -- Find exact matches
    let trivial = M.intersectionWith (\m h -> (m, h)) mouseLabels humanLabels
    printMatches $ M.elems trivial
    -- Find best matches for remaining labels
    let mouseRest = M.difference mouseLabels trivial
    let humanRest = M.difference humanLabels trivial
    let matches = nGramMatch quality mouseRest humanRest
    printMatches $ M.toList matches


-- Read file and decode to Text
readText :: String -> IO [Text]
readText path = T.lines . decodeUtf8 <$> BS.readFile path


parse :: Text -> Text -> [Text] -> Map Text Text
parse idPre labelPre = M.fromList . pairUp . ids
  where
    ids [] = []
    ids (l:ls) = case T.stripPrefix idPre l of
        Just ident -> T.take 10 ident : labels ls
        Nothing -> ids ls

    labels [] = []
    labels (l:ls) = case T.stripPrefix labelPre l of
        Just label -> T.takeWhile ('<' /=) label : ids ls
        Nothing -> labels ls

    pairUp [] = []
    pairUp [_] = []
    pairUp (i : l : xs) = (normalize l, i) : pairUp xs
    space t = T.replace t " "
    normalize = space "-" . space " or " . space " and " . space "/" . space "_" . T.toLower


printMatches :: [(Text, Text)] -> IO ()
printMatches = mapM_ (\(m, h) -> putStrLn $ T.unpack m ++ ", " ++ T.unpack h)
