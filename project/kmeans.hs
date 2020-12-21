{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}


import Control.Monad (when, forM)
import Data.Char(toUpper)
import Control.Applicative
import System.IO
import System.Random
import Data.List
import Data.List.Split
import Data.Double.Conversion.Text
import qualified Data.Text as T
import qualified Data.Map as M

---TESTING CODE WITH IO---

type ListDoubles = [Double] 

readFromFile :: IO ()
readFromFile = do
    theFile <- openFile "input.txt" ReadMode
    contents <- hGetContents theFile
    let k = 3
    let clus = 6
    let lsdouble = createListDoubles (lines contents)
    cent <- rnd_select_k lsdouble k
    let start = centroids cent
    let kout = kmeanclust k lsdouble start clus
    -- let kout = kmeans 4 lsdouble start
    -- print kout
    hClose theFile

--plot("outputKmeans.txt") with points palette

readTestData :: IO ()
readTestData = do
    theFile <- openFile "testdata.txt" ReadMode
    outFile <- openFile "output.txt" WriteMode
    contents <- hGetContents theFile
    let k = 10
    let clus = 3
    let listPoint = createListPoints (lines contents)
    cent <- rnd_select_k listPoint k
    let start = centroids cent
    let kout = kmeanclust k listPoint start clus
    --let kout = kmeans k listPoint start
    let tokout = kout
    let klist = kmapToList tokout
    let kstring = mergeStrings (list' ((length klist) - 1) klist)
    when (length kstring > 0) $
         writeFile "outputKmeans.txt" kstring
    hClose outFile
    hClose theFile

---IO FUNCTIONS--

kmapToList :: KOp a => M.Map a [a] -> [[a]]
kmapToList m = foldr (:) [] m

list' :: Int -> [[(Double,Double)]] -> [[String]]
list' 0 [] = []
list' 0 ls = []
list' v (x:xs) = (lsToStr v x):(list' (v - 1) xs)

lsToStr :: Int -> [(Double,Double)] -> [String]
lsToStr v ls = map (\(x1, y1) -> (show x1) ++ " " ++ (show y1) ++ " " ++ (show v)) ls 

mergeStrings :: [[String]] -> String
mergeStrings ls = unlines (concat ls)

createListPoints :: [String] -> [(Double, Double)]
createListPoints ls = map (\p -> getPoints (words p) ) ls

getPoints :: [String] -> (Double, Double)
getPoints ls = p (stringToDouble ls)

p :: [Double] -> (Double, Double)
p (p:ps:_) = (p,ps)
p (p:_)    = (p, 0.0)
p _        = (0.0, 0.0)

createListDoubles :: [String] -> ListDoubles
createListDoubles = stringToDouble . concatMap (splitOn ",") 

stringToDouble :: [String] -> [Double]
stringToDouble ls = (\x -> read x :: Double) <$> ls

--Kmeans Type Class--

class Ord a => KOp a where
    distance :: a -> a -> Double
    mean :: [a] -> a
    variance :: [a] -> a -> Double
    
instance KOp Double where
    distance a b  = abs (a - b)
    mean ls       = (foldr (+) (0.0) ls) / (fromIntegral (length ls)) 
    variance ls m = (foldr (\v -> (+) ((v - m)*(v - m))) (0.0) ls) / ((fromIntegral (length ls)))

instance KOp (Double, Double) where
    distance (x1, y1) (x2, y2) = sqrt (x*x + y*y)
                        where 
                            x = x1 - x2
                            y = y1 - y2
    mean        = xymean
    variance ps m = (\(x, y) -> x + y) (xyvarsum ps m)

---there's probably a better way to write this
xyvarsum :: [(Double, Double)] -> (Double, Double) -> (Double, Double)
xyvarsum ls m = foldr (\(x,y) (x1, y1) -> (((x1 + ((x - (fst m))*(x - (fst m))))),(y1 + ((y - (snd m))*(y - (snd m)))))) (0.0, 0.0) ls

xysum:: [(Double, Double)] -> (Double, Double)
xysum = foldr (\(x, y) (ox, oy) -> (x + ox, y + oy) ) (0,0) 

xymean :: [(Double, Double)] -> (Double, Double)
xymean ls = (\(x, y) -> (x / len, y / len) ) (xysum ls)
    where len = fromIntegral (length ls)

--Entry Point To Kmeans Clustered--

kmeanclust :: KOp a => Int -> [a] -> M.Map a [a] -> Int -> M.Map a [a]
kmeanclust k ps centroids numr = case (getbestVar (M.fromList kmeans') (getMinVar (kmeans')))  of 
                                    Just kk  -> kk
                                    Nothing  -> M.empty
                        where kmeans' = kmeanclust' k ps centroids numr

getbestVar :: KOp a => M.Map Double (M.Map a [a]) -> [Double] -> Maybe (M.Map a [a])
getbestVar ls vs = M.lookup (minimum vs) ls

getMinVar :: KOp a => [(Double, M.Map a [a])] -> [Double]
getMinVar = foldr (\(val, m) -> (:) val ) [] 

centroids :: KOp a => [a] -> M.Map a [a]
centroids ps = foldr (\val -> M.insert val []) M.empty ps

kmeanclust' :: KOp a => Int -> [a] -> M.Map a [a] -> Int -> [(Double, M.Map a [a])]
kmeanclust' k ps centroids numr = case numr of
                                    0 -> []
                                    r -> (var, kmean) : (kmeanclust' k ps centroids (numr - 1))
                                    where kmean = kmeans k ps centroids
                                          var   = calAllVar kmean

calAllVar :: KOp a => M.Map a [a] -> Double
calAllVar m = M.foldrWithKey (\key as -> (+) (variance as key)) 0.0 m

--Entry Point to Kmeans--
 
kmeans :: KOp a => Int -> [a] -> M.Map a [a] -> M.Map a [a]
kmeans k ps centroids = iterK (assignK centroids ps) ps
        
iterK :: KOp a => M.Map a [a] -> [a] -> M.Map a [a]
iterK clusters ps = case converge clusters of
                        (True, clust) -> clusters
                        (False, clust) -> iterK (assignK clust ps) ps

converge :: KOp a => M.Map a [a] -> (Bool, M.Map a [a])
converge = M.foldrWithKey (\a as (b, map1) -> (((a == (mean as)) && b), M.insert (mean as) as map1)) (True, M.empty)
            
assignK :: KOp a => M.Map a [a] -> [a] -> M.Map a [a]
assignK clust ps = foldr (\s m-> insertM s m) M.empty ps
            where insertM s m = case M.lookup (getclust (M.keys clust) s) m of
                                    Nothing  -> M.insert key [s] m
                                    Just ls  -> M.adjust (\ss -> (s:ss)) key m
                                    where key = getclust (M.keys clust) s

getclust :: KOp a => [a] -> a -> a
getclust keys d = getClosest (findDist keys d)

findDist :: KOp a => [a] -> a -> M.Map Double a
findDist ks val = foldr (\key -> M.insert (distance val key) key ) M.empty ks

getClosest :: KOp a => M.Map Double a -> a
getClosest m = snd (M.findMin m)

--Used only without randomness
 
splitBy :: Int -> [a] -> [[a]]
splitBy k ls = transpose (chunksOf k ls)

means :: KOp a => [[a]] -> M.Map a [a]
means = foldr (\ss -> M.insert (mean ss) ss) M.empty 

--Random!!!---

rnd_select_k :: [a] -> Int -> IO [a]
rnd_select_k _ 0 = return []
rnd_select_k (x:xs) n = do 
    r <- randomRIO (0, (length xs))
    if (r < n)
        then do
            rest <- rnd_select_k xs (n-1)
            return (x : rest)
        else rnd_select_k xs n
        