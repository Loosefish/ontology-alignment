{-# LANGUAGE OverloadedStrings #-}
module Match where

import Control.Parallel.Strategies
import Data.Array.IArray ((!))
import qualified Data.Array.IArray as A
import Data.Array.Unboxed (UArray)
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Munkres


chunked :: NFData a => Strategy [a]
chunked = parListChunk 16000 rdeepseq


findMatches :: Map Text Text -> Map Text Text -> [(Text, Text)]
findMatches one two = M.elems exact ++ best
  where
    exact = M.intersectionWith (\x y -> (x, y)) one two
    one' = M.difference one exact
    two' = M.difference two exact
    -- best = triGramMatch 0.536 one' two'  -- .84685340123681338667
    best = munkresMatch 0.536 one' two'  -- .84685340123681338667


-- Use 3-gram similarity to find matches
triGramMatch :: Float -> Map Text Text -> Map Text Text -> [(Text, Text)]
triGramMatch quality one two =
    map translate . greedyMatch . cutoff . matchList . variances $ similarities one two
  where
    cutoff = takeWhile (\(q, _, _) -> q > quality)
    translate (i1, i2) = (snd $ M.elemAt i1 one, snd $ M.elemAt i2 two)


-- Construct a matrix with similarity values between maps
similarities :: Map Text Text -> Map Text Text -> UArray (Int, Int) Float
similarities one two = A.listArray ((0, 0), (M.size one - 1, M.size two - 1)) elements
  where
    elements = [sim l1 l2 | l1 <- toTris one, l2 <- toTris two] `using` chunked
    toTris = map (nGrams 3 . pad) . M.keys
    pad text = T.append "  " $ T.append text "  "


-- Set of n-grams for a word
nGrams :: Int -> Text -> Set Text
nGrams _ "" = S.empty
nGrams 0 _ = S.empty
nGrams n word = S.fromList $ nGrams' word
  where
    nGrams' text
        | T.length text >= n = T.take n text : nGrams' (T.drop 1 text)
        | otherwise = []


-- Similarity of two sets
sim :: Ord a => Set a -> Set a -> Float
sim xs ys = size uni / size inter
  where
    (uni, inter) = unionIntersection xs ys
    size = fromIntegral . S.size


-- Compute union and intersection of two sets at the same time
unionIntersection :: Ord a => Set a -> Set a -> (Set a, Set a)
unionIntersection one two = unionIntersection' [] [] two (S.toList one)
  where
    unionIntersection' :: Ord a => [a] -> [a] -> Set a -> [a] -> (Set a, Set a)
    unionIntersection' is un s [] = (S.fromList is, S.fromList $ un ++ S.toList s)
    unionIntersection' is un s allX@(x : xs)
        | S.null s =  (S.fromList is, S.fromList $ un ++ allX)
        | x `S.member` s = unionIntersection' (x : is) (x : un) (S.delete x s) xs
        | otherwise = unionIntersection' is (x : un) s xs


-- Calculate variance for each entry
variances :: UArray (Int, Int) Float -> UArray (Int, Int) Float
variances matrix = A.listArray (A.bounds matrix) $ map freq $ A.assocs matrix
  where
    freq ((i1, i2), v) = v - rowAvgs !! i1 - colAvgs !! i2 + avg
    (rowAvgs, colAvgs, avg) = averages matrix


-- Calculate row, column and total average(s)
averages :: UArray (Int, Int) Float -> ([Float], [Float], Float)
averages m = (xAvgs, yAvgs, sum xAvgs / fromIntegral (xMax - xMin + 1))
  where
    ((xMin, yMin), (xMax, yMax)) = A.bounds m
    xAvgs = map xAvg [xMin..xMax]
    yAvgs = map yAvg [yMin..yMax]
    xAvg x = sum [m ! (x, i) | i <- [yMin..yMax]] / fromIntegral (yMax - yMin + 1)
    yAvg y = sum [m ! (i, y) | i <- [xMin..xMax]] / fromIntegral (xMax - xMin + 1)


-- Convert matrix to sorted list of possible matchings
matchList :: UArray (Int, Int) Float -> [(Float, Int, Int)]
matchList m = sortBy (flip compare) $ map (\((one, two), v) -> (v, one, two)) $ A.assocs m


-- Compute a matching by greedily adding candidates from list
greedyMatch :: (Ord a, Ord b) => [(a, b, b)] -> [(b, b)]
greedyMatch [] = []
greedyMatch (c:cs) = (id1, id2) : greedyMatch cs'
  where
    (_, id1, id2) = c
    cs' = filter (\(_, i1, i2) -> i1 /= id1 && i2 /= id2) cs
    

testM :: UArray (Int, Int) Float
testM = A.amap (1.0 - ) $ A.array ((1,1), (4,4))
    [ ((1,1), 0.5)
    , ((1,2), 0.8)
    , ((1,3), 0.2)
    , ((1,4), 0.2)

    , ((2,1), 0.6)
    , ((2,2), 0.7)
    , ((2,3), 0.1)
    , ((2,4), 0.3)

    , ((3,1), 0.7)
    , ((3,2), 0.3)
    , ((3,3), 0.4)
    , ((3,4), 0.8)
    ]


m1 :: Map Text Text
m1 = M.fromList [("foo", "a"), ("bar", "b"), ("baz", "c")]
m2 :: Map Text Text
m2 = M.fromList [("barr", "x"), ("buzz", "y"), ("xyxyl", "z"), ("foo", "q")]



similarities' :: Map Text Text -> Map Text Text -> UArray (Int, Int) Float
similarities' one two = A.amap (1.0 -) $ A.array ((1, 1), (size, size)) elements
  where
    size = max (M.size one) (M.size two)
    elements = [((i1, i2), sim l1 l2) | (i1, l1) <- toTris one, (i2, l2) <- toTris two] `using` chunked
    toTris = zip [1..] . map (nGrams 3 . pad) . M.keys
    pad text = T.append "  " $ T.append text "  "


munkresMatch :: Float -> Map Text Text -> Map Text Text -> [(Text, Text)]
munkresMatch cutoff one two = map (\(i2, i1) -> (valueAt i1 one, valueAt i2 two)) matchList
  where
    valueAt i = snd . M.elemAt (i - 1)
    sims = similarities' one two
    sizeOne = M.size one
    sizeTwo = M.size two
    quality i1 i2 = 1.0 - sims ! (i1, i2)
    matchList = filter (\(i2, i1) -> i1 <= sizeOne && i2 <= sizeTwo && quality i1 i2 > cutoff) $ fst $ hungarianMethodFloat sims



-- munkresMatch :: UArray (Int, Int) Float -> [(Float, Int, Int)]
-- munkresMatch sims = map (\(i2, i1) -> (sims ! (i1, i2), i1, i2)) matchList
--   where
--     (matchList, _) = hungarianMethodFloat sims
