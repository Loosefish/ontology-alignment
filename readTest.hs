{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS


idMouse :: Text
idMouse = "    <owl:Class rdf:about=\"http://mouse.owl#"
idHuman :: Text
idHuman = "    <owl:Class rdf:about=\"http://human.owl#"
labelAny :: Text
labelAny = "        <rdfs:label rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\">"


-- .55 -> .8255
quality :: Float
quality = 0.56


ngrams :: Int -> Text -> Set Text
ngrams _ "" = S.empty
ngrams 0 _ = S.empty
ngrams n word = S.fromList $ ngrams' word
  where
    ngrams' text
        | T.length text >= n = T.take n text : ngrams' (T.drop 1 text)
        | otherwise = []


triGrams :: Text -> Set Text
triGrams = ngrams 3


sim :: Text -> Text -> Float
sim x y = floatSize (x' `S.intersection` y') / floatSize (x' `S.union` y')
  where
    floatSize = fromIntegral . S.size
    [x', y'] = map triGrams [x, y]


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


center :: Text -> Text
center text = T.append "  " $ T.append text "  "


match :: Map Text Text -> Map Text Text -> Map Text Text
match one two = match' candidates M.empty
  where
    one' = M.toList one
    two' = M.toList two
    allMatches = [(sim l1 l2, id1, id2) | (l1, id1) <- one', (l2, id2) <- two']
    candidates = sortBy (flip compare) $ filter (\(q, _, _) -> q > quality) allMatches


match' :: [(Float, Text, Text)] -> Map Text Text -> Map Text Text
match' [] matches = matches
match' (c:cs) matches
    | M.member id1 matches = match' cs matches
    | otherwise = match' cs (M.insert id1 id2 matches)
  where
    (_, id1, id2) = c


main :: IO ()
main = do
    -- Read input files
    mouseIds <- parse idMouse labelAny <$> readText "mouse.owl"
    humanIds <- parse idHuman labelAny <$> readText "human.owl"
    -- Find exact matches
    let trivial = M.intersectionWith (\m h -> (m, h)) mouseIds humanIds
    printMatches $ M.elems trivial
    -- Find best matches for remaining
    let mouseIds' = M.mapKeys center $ M.difference mouseIds trivial
    let humanIds' = M.mapKeys center $ M.difference humanIds trivial
    -- let matches = bestMatches quality mouseIds' humanIds'
    let matches = match mouseIds' humanIds'
    printMatches $ M.toList matches
