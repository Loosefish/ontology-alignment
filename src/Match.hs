{-# LANGUAGE OverloadedStrings #-}
module Match (findMatches) where

import Control.Arrow (first)
import Control.Parallel.Strategies
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T


findMatches :: Map Text Text -> Map Text Text -> [(Text, Text)]
findMatches one two = M.elems exact ++ best
  where
    exact = M.intersectionWith (\x y -> (x, y)) one two
    one' = M.difference one exact
    two' = M.difference two exact
    best = M.toList $ nGramMatch 0.545 one' two'


-- Use n-gram similarity to find matches
nGramMatch :: Float -> Map Text Text -> Map Text Text -> Map Text Text
nGramMatch quality one two = greedyMatch candidates M.empty
  where
    toTris = map (first $ nGrams 3 . pad) . M.toList
    allMatches = [(sim l1 l2, id1, id2) | (l1, id1) <- toTris one, (l2, id2) <- toTris two] `using` parListChunk 16000 rdeepseq
    candidates = sortBy (flip compare) $ filter (\(q, _, _) -> q > quality) allMatches


-- Compute a matching by greedily adding candidates
greedyMatch :: Ord a => [(a, Text, Text)] -> Map Text Text -> Map Text Text
greedyMatch [] matches = matches
greedyMatch (c:cs) matches = greedyMatch cs' (M.insert id1 id2 matches)
  where
    (_, id1, id2) = c
    cs' = filter (\(_, i1, i2) -> i1 /= id1 && i2 /= id2) cs


-- Surround text with two spaces on each side
pad :: Text -> Text
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
sim :: Set Text -> Set Text -> Float
sim xs ys = size uni / size inter
  where
    (uni, inter) = unionIntersection xs ys
    -- (uni, inter) = (S.union xs ys, S.intersection xs ys)
    size = fromIntegral . S.size


-- Compute union and intersection of two sets at the same time
unionIntersection :: Set Text -> Set Text -> (Set Text, Set Text)
unionIntersection one two = unionIntersection' [] [] two (S.toList one)
  where
    unionIntersection' :: [Text] -> [Text] -> Set Text -> [Text] -> (Set Text, Set Text)
    unionIntersection' is un s [] = (S.fromList is, S.fromList $ un ++ S.toList s)
    unionIntersection' is un s allX@(x : xs)
        | S.null s =  (S.fromList is, S.fromList $ un ++ allX)
        | x `S.member` s = unionIntersection' (x : is) (x : un) (S.delete x s) xs
        | otherwise = unionIntersection' is (x : un) s xs
