
-- | The Munkres version of the Hungarian Method for weighted minimal 
-- bipartite matching. 
-- The implementation is based on Robert A. Pilgrim's notes, 
-- <http://216.249.163.93/bob.pilgrim/445/munkres.html>
-- (mirror: <http://www.public.iastate.edu/~ddoty/HungarianAlgorithm.html>).

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleContexts #-}
module Munkres
  ( hungarianMethodInt
  , hungarianMethodFloat
  , hungarianMethodDouble
  , hungarianMethodBoxed
  ) where

import Prelude hiding (flip)
  
import Control.Monad
import Control.Monad.ST

import Data.List hiding (insert)

import Data.STRef
import Data.Array.ST

import Data.Array.IArray ()
import Data.Array.MArray
import Data.Array.Unboxed

-------------------------------------------------------

swap :: (Int,Int) -> (Int,Int)
swap (x,y) = (y,x)

{-
complementSort :: Int -> [Int] -> [Int]
complementSort n xs = complement n (sort xs)
-}

-- assumes that the input is sorted
complement :: Int -> [Int] -> [Int]
complement n list = worker 1 list where
  worker k xxs@(x:xs) = if k>n 
    then []
    else case compare k x of
      EQ -> worker (k+1) xs
      LT -> k : worker (k+1) xxs
      GT -> worker k xs
  worker k [] = [k..n]


-- assumes that the inputs are sorted sets 
mergeUnion :: [Int] -> [Int] -> [Int]
mergeUnion xxs@(x:xs) yys@(y:ys) = case compare x y of
  LT -> x : mergeUnion xs yys
  EQ -> x : mergeUnion xs  ys
  GT -> y : mergeUnion xxs ys 
mergeUnion xs [] = xs
mergeUnion [] ys = ys

insert :: Int -> [Int] -> [Int]
insert y xxs@(x:xs) = case compare y x of
  LT -> y : xxs 
  EQ -> xxs
  GT -> x : insert y xs 
insert y [] = [y]

remove :: Int -> [Int] -> [Int]
remove y xxs@(x:xs) = case compare y x of
  LT -> xxs
  EQ -> xs
  GT -> x : remove y xs 
remove _ [] = []

{-# SPECIALIZE firstJust :: [ ST s (Maybe (Int,Int)) ] -> ST s (Maybe (Int,Int)) #-}
firstJust :: Monad m => [ m (Maybe a) ] -> m (Maybe a)
firstJust (a:as) = do
  x <- a
  case x of
    Just _ -> return x
    Nothing -> firstJust as
firstJust [] = return Nothing

{-# SPECIALISE alternate :: [Int] -> ([Int],[Int]) #-}
alternate :: [a] -> ([a],[a])
alternate list = flip list [] [] where
  flip (x:xs) ys zs = flop xs (x:ys) zs
  flip [] ys zs = (reverse ys,reverse zs)
  flop (x:xs) ys zs = flip xs ys (x:zs)
  flop [] ys zs = (reverse ys,reverse zs)

-------------------------------------------------------

-- polymorphicity problem workaround experiment...

thawST :: (IArray a e, MArray (STArray s) e (ST s)) => a (Int,Int) e -> ST s (STArray s (Int,Int) e) 
thawST = thaw

thawSTU :: (IArray UArray e, MArray (STUArray s) e (ST s)) => UArray (Int,Int) e -> ST s (STUArray s (Int,Int) e) 
thawSTU = thaw

newSTArray_ :: MArray (STArray s) e (ST s) => ((Int,Int),(Int,Int)) -> ST s (STArray s (Int,Int) e)
newSTArray_ = newArray_

newSTUArray_ :: MArray (STUArray s) e (ST s) => ((Int,Int),(Int,Int)) -> ST s (STUArray s (Int,Int) e)
newSTUArray_ = newArray_

-------------------------------------------------------

{- SPECIALISE hungarianMethod :: UArray (Int,Int) Int    -> ([(Int,Int)],Int   ) -}
{- SPECIALISE hungarianMethod :: UArray (Int,Int) Float  -> ([(Int,Int)],Float ) -}
{- SPECIALISE hungarianMethod :: UArray (Int,Int) Double -> ([(Int,Int)],Double) -}

-- | Needs a rectangular array of /nonnegative/ weights, which
-- encode the weights on the edges of a (complete) bipartitate graph.
-- The indexing should start from @(1,1)@.
-- Returns a minimal matching, and the cost of it.
-- 
-- Unfortunately, GHC is opposing hard the polymorphicity of this function. I think
-- the main reasons for that is that the there is no @Unboxed@ type class, and
-- thus the contexts @IArray UArray e@ and @MArray (STUArray s) e (ST s)@ do not
-- know about each other. (And I have problems with the @forall s@ part, too).

hungarianMethodInt :: UArray (Int,Int) Int -> ([(Int,Int)],Int) 
hungarianMethodInt input = runST $ do
  let ((1,1),(n,m)) = bounds input
  star <- if m >= n 
    then do 
      ar <- thawSTU input
      hungarianMethodShared ar
    else do
      ar <- newSTUArray_ ((1,1),(m,n))  
      forM_ [ (i,j) | i<-[1..n] , j<-[1..m] ] $ \(i,j) -> do
        writeArray ar (j,i) $ input ! (i,j) 
      star' <- hungarianMethodShared ar
      return (map swap star') 
  let costs = [ input ! ij | ij <- star ]
  return (star, sum costs)

hungarianMethodFloat :: UArray (Int,Int) Float -> ([(Int,Int)],Float) 
hungarianMethodFloat input = runST $ do
  let ((1,1),(n,m)) = bounds input
  star <- if m >= n 
    then do 
      ar <- thawSTU input
      hungarianMethodShared ar
    else do
      ar <- newSTUArray_ ((1,1),(m,n)) 
      forM_ [ (i,j) | i<-[1..n] , j<-[1..m] ] $ \(i,j) -> do
        writeArray ar (j,i) $ input ! (i,j) 
      star' <- hungarianMethodShared ar
      return (map swap star') 
  let costs = [ input ! ij | ij <- star ]
  return (star, sum costs)

hungarianMethodDouble :: UArray (Int,Int) Double -> ([(Int,Int)],Double) 
hungarianMethodDouble input = runST $ do
  let ((1,1),(n,m)) = bounds input
  star <- if m >= n 
    then do 
      ar <- thawSTU input
      hungarianMethodShared ar
    else do
      ar <- newSTUArray_ ((1,1),(m,n)) 
      forM_ [ (i,j) | i<-[1..n] , j<-[1..m] ] $ \(i,j) -> do
        writeArray ar (j,i) $ input ! (i,j) 
      star' <- hungarianMethodShared ar
      return (map swap star') 
  let costs = [ input ! ij | ij <- star ]
  return (star, sum costs)

-- | The same as 'hungarianMethod<Type>', but uses boxed values (thus works with
-- any data type which an instance of 'Real'). 
-- The usage of one the unboxed versions is recommended where possible, 
-- for performance reasons.
hungarianMethodBoxed :: (Real e, IArray a e) => a (Int,Int) e -> ([(Int,Int)],e)
hungarianMethodBoxed input = runST $ do
  let ((1,1),(n,m)) = bounds input
  star <- if m >= n 
    then do 
      ar <- thawST input -- :: ST s (STArray s (Int,Int) e)
      hungarianMethodShared ar
    else do
      ar <- newSTArray_ ((1,1),(m,n)) -- :: ST s (STArray s (Int,Int) e)
      forM_ [ (j,i) | j<-[1..m] , i<-[1..n] ] $ \(j,i) ->
        writeArray ar (j,i) $ input ! (i,j) 
      star' <- hungarianMethodShared ar
      return (map swap star') 
  let costs = [ input ! ij | ij <- star ]
  return (star, sum costs)


{-# SPECIALISE hungarianMethodShared :: STUArray s (Int,Int) Int    -> ST s [(Int,Int)] #-}
{-# SPECIALISE hungarianMethodShared :: STUArray s (Int,Int) Float  -> ST s [(Int,Int)] #-}
{-# SPECIALISE hungarianMethodShared :: STUArray s (Int,Int) Double -> ST s [(Int,Int)] #-}
  
hungarianMethodShared :: (Real e, MArray a e (ST s)) => a (Int,Int) e -> ST s [(Int,Int)]    
hungarianMethodShared ar = do
  starred <- newSTRef []
  primed  <- newSTRef []
  coveredRows <- newSTRef []
  coveredCols <- newSTRef []
  ((1,1),nm) <- getBounds ar
  munkers ar nm starred primed coveredRows coveredCols

-- the meat comes here...

{-# SPECIALISE munkers :: 
     STUArray s (Int,Int) Int -> (Int,Int) 
  -> STRef s [(Int,Int)] -> STRef s [(Int,Int)] 
  -> STRef s [Int] -> STRef s [Int]
  -> ST s [(Int,Int)] #-}
  
{-# SPECIALISE munkers :: 
     STUArray s (Int,Int) Float -> (Int,Int)  
  -> STRef s [(Int,Int)] -> STRef s [(Int,Int)] 
  -> STRef s [Int] -> STRef s [Int]
  -> ST s [(Int,Int)] #-}
  
{-# SPECIALISE munkers :: 
     STUArray s (Int,Int) Double -> (Int,Int)  
  -> STRef s [(Int,Int)] -> STRef s [(Int,Int)] 
  -> STRef s [Int] -> STRef s [Int]
  -> ST s [(Int,Int)] #-}
  
munkers :: (Real e, MArray a e (ST s)) 
  => a (Int,Int) e -> (Int,Int) 
  -> STRef s [(Int,Int)] -> STRef s [(Int,Int)] 
  -> STRef s [Int] -> STRef s [Int]
  -> ST s [(Int,Int)]   

munkers ar (n,m) starred primed coveredRows coveredCols = (step1 >> step2 >> step3) where

  kk = min n m

  step3 = do
    colsC <- readSTRef coveredCols
    star <- readSTRef starred
    let colsC' = mergeUnion colsC (sort $ map snd star)
    if length colsC' == kk
      then return star
      else do
        writeSTRef coveredCols colsC'
        step4
        
  step4 = do
    rowsC <- readSTRef coveredRows
    colsC <- readSTRef coveredCols
    let rowsNC = complement n rowsC
        colsNC = complement m colsC
    star <- readSTRef starred   
    let f ij = do
          x <- readArray ar ij 
          if x==0 then return (Just ij) else return Nothing 
    mp <- firstJust [ f (i,j) | i<-rowsNC, j<-colsNC ] 
    case mp of
      Nothing -> do
        es <- forM [ (i,j) | i<-rowsNC, j<-colsNC ] $ \ij -> readArray ar ij
        step6 (minimum es)   
      Just ij@(i,_) -> do
        modifySTRef primed (ij:) 
        case find (\(p,_) -> p==i) star of
          Nothing -> step5 ij
          Just (_,q) -> do
            modifySTRef coveredRows (insert i) 
            modifySTRef coveredCols (remove q) 
            step4

  step5 pq = do
    star <- readSTRef starred
    prim <- readSTRef primed
    alt <- step5a star prim pq [pq]
    let (ps,ss) = alternate alt
    writeSTRef starred $ (star \\ ss) ++ ps
    writeSTRef primed []
    writeSTRef coveredRows []
    writeSTRef coveredCols []
    step3 
    
  step5a :: [(Int,Int)] -> [(Int,Int)] -> (Int,Int) -> [(Int,Int)] -> ST s [(Int,Int)]
  step5a star prim (_,q) xs = 
    case findStarred q of
      Just (i,_) -> do
        let (_,j) = findPrimed i
        step5a star prim (i,j) ((i,j):(i,q):xs)
      Nothing -> return xs
    where
      findStarred j =      find (\(_,c) -> (c==j)) star
      findPrimed  i = case find (\(r,_) -> (r==i)) prim of
        Just x  -> x
        Nothing -> error $ "Munkres/findPrimed: should not happen (" ++ show prim ++ " " ++ show i ++ ")"
        
  step2 = 
    do 
      s <- foldM worker [] [ (i,j) | i<-[1..n], j<-[1..m] ] 
      writeSTRef starred s
    where
      worker star ij@(i,j) = do
        x <- readArray ar ij
        if x==0 
          then case filter (\(a,b) -> (a==i) || (b==j)) star of
            [] -> return (ij : star)
            _ -> return star
          else return star

  step6 c = do
    rowsC <- readSTRef coveredRows
    colsC <- readSTRef coveredCols
    let rowsNC = complement n rowsC
        colsNC = complement m colsC
    forM rowsNC $ \i -> 
      forM colsNC $ \j -> do
        x <- readArray ar (i,j)
        writeArray ar (i,j) (x-c)
    forM rowsC $ \i -> 
      forM colsC $ \j -> do
        x <- readArray ar (i,j)
        writeArray ar (i,j) (x+c)
    step4
        
  step1 = mapM_ subRow [1..n]
  subRow i = do
    row <- forM [1..m] $ \j -> readArray ar (i,j)
    let y = minimum row
    forM [1..m] $ \j -> do
      let ij = (i,j)
      x <- readArray ar ij
      writeArray ar ij (x-y)
