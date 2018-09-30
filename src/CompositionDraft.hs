-- /!\ THIS MODULE IS FOR DEAD-CODE ARCHIVING ONLY /!\
module CompositionDraft where

import Data.Ratio
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Tree

import Control.Monad.Random

import System.Random.Shuffle

import Util

perfect :: [Rational]
perfect =
  -- perfect consonances
  [ 1%1 -- unison
  , 1%2 -- octave
  , 3%4 -- perfect fourth
  , 2%3 -- perfect fifth
  ]
  
imperfect :: [Rational]
imperfect =
  -- imperfect consonances
  [ 4%5 -- major third
  , 5%8 -- minor sixth
  , 5%6 -- minor third  
  , 3%5 -- major sixth
  ]

consonances :: [Rational]
consonances = perfect ++ imperfect

isConsonant :: Rational -> Rational -> Bool
isConsonant a b = any f consonances
  where
    n = min a b
    d = max a b
    f x = isPowerOfTwo' (n/d/x) || isPowerOfTwo' (d/n*x)

notes :: Int -> Rational -> [Rational]
notes n lo = nub $ pow n 1
  where
    pow 0 x = return x
    pow n x = consonances >>= pow (n-1) . clamp
      where
        clamp y | x*y < lo  = x*y*2
                | otherwise = x*y

chords_ :: Int -> [Rational] -> [[Rational]]
chords_ n notes = nub $ pow n []
  where
    pow 0 xs = return xs
    pow n xs = do
      y <- notes
      guard $ all (isConsonant y) xs
      pow (n-1) $ y:xs


enumerate :: [[a]] -> [[a]]
enumerate []     = [[]]
enumerate (x:xs) = concat $ map f x
  where f y = map (y:) (enumerate xs)

chords :: [[[Rational]]]
chords = deduplicate $ [] : f [[]]
  where
    deduplicate = map $
      (map S.toDescList) . S.toDescList . S.fromList . (map S.fromList)
    f n_1 = let n = g n_1
            in if null n
               then []
               else n : f n
    g n_1 = [ x:xs | x  <- consonances
                   , xs <- n_1
                   , x `notElem` xs
                   , and $ map (isConsonant x) xs ]

chords' :: [[[Rational]]]
chords' = map (uncurry f) $ zip [0..] chords
  where
    f n = nub
          . concatMap (enumerate . replicate n)
          . concatMap (zipWith (zipWith ($)) fs . repeat)
      where
        fs = enumerate $ replicate n [id,(*2)]

consonantGraph :: [Rational] -> M.Map [Rational] [[Rational]]
consonantGraph maxGaps = M.fromListWith (++)
  [(a,[b]) | a <- xs, b <- xs, isConsonant' a b, isClose a b]
  where
    xs = chords' !! length maxGaps
    isConsonant' a b = and $ zipWith isConsonant a b
    isClose a b = and $ zipWith3 (\g x y -> max x y / min x y <= g) maxGaps a b

 

consonantCycles :: MonadInterleave m => Int -> Rational -> Rational -> Rational -> [Int] -> (Rational -> Rational) -> m [[[Rational]]]
consonantCycles n lo hi imperfectRate ds minSkipped = interleave $ map (chunksOf n) <$> tree n 0 ds IM.empty 1
  where
    perfect'       = nub $ perfect   ++ map (1/) perfect
    imperfect'     = nub $ imperfect ++ map (1/) imperfect
    insert' n r cs = IM.insertWith (++) n [r] cs

    tree :: MonadInterleave m => Int -> Int -> [Int] -> (IM.IntMap [Rational]) -> Rational -> m [[Rational]]
    tree _ _ [] _  _ = return [[]]
    tree 0 _ ds cs 1 = tree n 0 (tail ds) cs 1
    tree 0 _ _  _  _ = return []
    
    tree n 0 ds cs r
      | r < lo = tree n 0 ds cs (r*2)
      | r > hi = tree n 0 ds cs (r/2)
      | maybe True (all $ isConsonant r) (IM.lookup n cs) = skip n ds $ do
          rd        <- getRandom
          ri        <- getRandomR (0, length imperfect' - 1)
          let ip    =  if rd <= imperfectRate then [imperfect'!!ri] else []
          next      <- map (r*) <$> shuffleM (ip ++ perfect')
          let tree' =  tree (n-1) (head ds - 1) ds (insert' n r cs)
          children  <- mapM tree' next
          return $ map (r:) $ concat children
      | otherwise = return []
      
    tree n d ds cs r = skip n ds $ map (r:) <$> tree (n-1) (d-1) ds (insert' n r cs) r

    skip ln lds x = interleave $ do
      r <- getRandom
      if depth == 0 || r >= minSkipped (toRational depth / toRational maxDepth)
        then x
        else return []
      where
        m        = length ds
        lm       = length lds
        depth    = maxDepth - (lm - 1) - (ln - 1) * m
        maxDepth = n * m - 1


consonantCycles' :: [[[Rational]]]
consonantCycles' = evalRand (([]:) <$> mapM enumerate [1..]) (mkStdGen 42)
  where
    node label = interleave $ do
      next <- shuffleM all
      children <- mapM node next
      return $ Node label children
      where
        up   = map (label/) consonances
        down = map (label*) consonances
        all  = filter (label/=) $ up ++ down
    
    enumerate n = interleave $ f n <$> node 1
      where
        f 0 (Node 1 _) = [[]]
        f 0 (Node _ _) = []
        f n (Node label children) = map (label:) $ concatMap (f $ n-1) children


-- WARNING: maxAlterations is too costly! consider to remove it.
consonantCycles'' :: MonadInterleave m =>
  Int -> (Rational -> Rational) -> [Rational] -> m [[[Rational]]]
consonantCycles'' n minSkipped maxGaps = interleave $ enumerateTree n ones
  where
    ones = map (const 1) maxGaps
    graph = consonantGraph maxGaps
    
    enumerateTree 0 root = interleave $ skip 0 $
      return $ if root == ones then [[]] else []
    enumerateTree depth root = interleave $ skip depth $ do
      next <- shuffleM $ graph M.! root
      children <- mapM (enumerateTree $ depth-1) next
      return $ map (root:) $ concat children

    skip depth x
      | depth == n = x
      | otherwise = do
          r <- getRandom
          if r >= minSkipped (1 - fromIntegral depth / fromIntegral (n-1))
            then x
            else return []

    -- skip _ _       []     = []
    -- skip m skipped (x:xs) = if skipped/m < minSkipped
    --                         then skip (m+1) (skipped+1) xs
    --                         else x : skip (m+1) skipped xs
      
    --   if not valid
    --   then skip (m+1) (skipped+1) xs
    --   else if skipped/m < minSkipped
    --        then skip (m+1) (skipped+1) xs
    --        else x : skip (m+1) skipped xs
    --   where
    --     valid = and
    --       $ zipWith (>=) maxAlterations
    --       $ map ((/ fromIntegral (n-1)) . alterations)
    --       $ transpose x

    -- alterations []     = 0
    -- alterations (x:xs) = fst $ foldl f (0,x) xs
    --   where
    --     f (n,a) b = if a == b then (n,b) else (n+1,b)


consonantCycles''' :: MonadInterleave m =>
  Int -> Int -> (Rational -> Rational) -> m [[[Rational]]]
consonantCycles''' n m minSkipped = interleave $ enumerateTree n ones
  where
    ones = replicate m 1
    mchords = chords !! m ++ map (map (1/)) (chords !! m)
    
    enumerateTree 0 root = skip 0 $ if root == ones then [[]] else []
    enumerateTree depth root = interleave $ do
      next <- shuffleM $ map (zipWith (*) root) mchords
      children <- mapM (enumerateTree $ depth-1) next
      skip depth $ map (root:) $ concat children

    skip depth x = do
      r <- getRandom
      if r >= minSkipped (1 - fromIntegral depth / fromIntegral n)
        then return x
        else return []
