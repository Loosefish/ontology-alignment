{-# LANGUAGE OverloadedStrings #-}
module Match (findMatches) where

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


type Label = Text
type Id = Text
type Annotations = Map Label (Id, [Label])


chunked :: NFData a => Strategy [a]
chunked = parListChunk 16000 rdeepseq


findMatches :: Float -> Annotations -> Annotations -> [(Id, Id)]
findMatches cutoff one two = M.elems exact ++ best
  where
    exact = M.intersectionWith (\x y -> (fst x, fst y)) one two
    one' = M.difference one exact
    two' = M.difference two exact
    best = triGramMatch cutoff one' two'  -- .84685340123681338667


-- Use 3-gram similarity to find matches
triGramMatch :: Float -> Annotations -> Annotations -> [(Id, Id)]
triGramMatch quality one two =
    map translate . greedyMatch . cutoff . matchList . variances $ similarities one two
  where
    cutoff = takeWhile (\(q, _, _) -> q > quality)
    translate (i1, i2) = (fst . snd $ M.elemAt i1 one, fst . snd $ M.elemAt i2 two)


-- Construct a matrix with similarity values between maps
similarities :: Annotations -> Annotations -> UArray (Int, Int) Float
similarities one two = A.listArray ((0, 0), (M.size one - 1, M.size two - 1)) elements
  where
    elements = mapPairs bagSim (M.toAscList one) (M.toAscList two) `using` chunked
    bagSim x y = maximum $ mapPairs sim (toTris x) (toTris y)
    toTris (label, (_, synonyms)) = map (nGrams 3) (label : synonyms)


-- Set of n-grams for a word
nGrams :: Int -> Text -> Set Text
nGrams _ "" = S.empty
nGrams 0 _ = S.empty
nGrams n word = S.fromList $ nGrams' word
  where
    nGrams' text
        | T.length text >= n = T.take n text : nGrams' (T.drop 1 text)
        | otherwise = []


-- Apply a function to all pairs of elements from two lists
mapPairs :: (a -> b -> c) -> [a] -> [b] -> [c]
mapPairs f xs ys = [f x y | x <- xs, y <- ys]


-- Similarity of two sets
sim :: Ord a => Set a -> Set a -> Float
sim xs ys = size inter / size uni
  where
    (inter, uni) = unionIntersection xs ys
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


-- Calculate variances for each entry
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
