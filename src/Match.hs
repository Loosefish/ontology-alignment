{-# LANGUAGE OverloadedStrings #-}
module Match
    ( nGramMatch
    , distMatch)
where
import Control.Arrow (first)
import Control.Parallel.Strategies
import Data.Array
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S


-- Set of n-grams for a word
nGrams :: Int -> Text -> Set Text
nGrams _ "" = S.empty
nGrams 0 _ = S.empty
nGrams n word = S.fromList $ nGrams' word
  where
    nGrams' text
        | T.length text >= n = T.take n text : nGrams' (T.drop 1 text)
        | otherwise = []


-- Surround text with two spaces on each side
pad :: Text -> Text
pad text = T.append "  " $ T.append text "  "


-- Similarity of two sets
sim :: Set Text -> Set Text -> Float
sim xs ys = size uni / size inter
  where
    (uni, inter) = unionIntersection xs ys
    size = fromIntegral . S.size


-- Calculate union and intersection of two sets at the same time
unionIntersection :: Set Text -> Set Text -> (Set Text, Set Text)
unionIntersection one two = unionIntersection' [] [] two (S.toList one)
  where
    unionIntersection' :: [Text] -> [Text] -> Set Text -> [Text] -> (Set Text, Set Text)
    unionIntersection' is un s [] = (S.fromList is, S.fromList $ un ++ S.toList s)
    unionIntersection' is un s allX@(x : xs)
        | S.null s =  (S.fromList is, S.fromList $ un ++ allX)
        | x `S.member` s = unionIntersection' (x : is) (x : un) (S.delete x s) xs
        | otherwise = unionIntersection' is (x : un) s xs


nGramMatch :: Float -> Map Text Text -> Map Text Text -> Map Text Text
nGramMatch quality one two = greedyMatch candidates M.empty
  where
    toTris = map (first $ nGrams 3 . pad) . M.toList
    allMatches = [(sim l1 l2, id1, id2) | (l1, id1) <- toTris one, (l2, id2) <- toTris two] `using` parListChunk 16000 rdeepseq
    candidates = sortBy (flip compare) $ filter (\(q, _, _) -> q > quality) allMatches


distMatch :: Int -> Map Text Text -> Map Text Text -> Map Text Text
distMatch quality one two = greedyMatch candidates M.empty
  where
    toSeq = map (first T.unpack) . M.toList
    allMatches = [(dist l1 l2, id1, id2) | (l1, id1) <- toSeq one, (l2, id2) <- toSeq two]
    candidates = sort $ filter (\(q, _, _) -> q < quality) allMatches


-- Generate a matching by greedily adding candidates
greedyMatch :: Ord a => [(a, Text, Text)] -> Map Text Text -> Map Text Text
greedyMatch [] matches = matches
greedyMatch (c:cs) matches = greedyMatch cs' (M.insert id1 id2 matches)
  where
    (_, id1, id2) = c
    cs' = filter (\(_, i1, i2) -> i1 /= id1 && i2 /= id2) cs


dist :: (Eq a) => [a] -> [a] -> Int
dist xs ys = levMemo ! (n, m)
  where levMemo = array ((0,0),(n,m)) [((i,j), lev i j) | i <- [0..n], j <- [0..m]]
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        lev 0 v = v
        lev u 0 = u
        lev u v
          | xa ! u == ya ! v = levMemo ! (u-1, v-1)
          | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                            levMemo ! (u-1, v),
                                            levMemo ! (u-1, v-1)] 
