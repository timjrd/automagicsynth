module Composition where

import Data.Maybe
import Data.Ratio
import Data.List
import Data.Tree
import qualified Data.IntMap as M

import Control.Monad.Random.Lazy

import System.Random.Shuffle

import Constraint
import Util

data Class = Perfect | Imperfect
  deriving Show

type Consonance = Rational
type Pitch      = Rational

perfect :: [Consonance]
perfect =
  -- perfect consonances
  [ 1%1 -- unison
  , 1%2 -- octave
  , 3%4 -- perfect fourth
  , 2%3 -- perfect fifth
  ]
  
imperfect :: [Consonance]
imperfect =
  -- imperfect consonances
  [ 4%5 -- major third
  , 5%8 -- minor sixth
  , 5%6 -- minor third  
  , 3%5 -- major sixth
  ]

consonances :: [Consonance]
consonances = perfect ++ imperfect

isConsonant :: Pitch -> Pitch -> Bool
isConsonant a b = any f consonances
  where
    n = min a b
    d = max a b
    f x =
      isPowerOfTwo' (n/d/x) ||
      isPowerOfTwo' (d/n*x)

mapClass :: Constraint () () Class
mapClass = Constraint () (repeat id) (repeat f)
  where f _ _ = [((),Perfect), ((),Imperfect)]

filterImperfect :: [Int] -> Constraint Int Class Class
filterImperfect ms = Constraint 0 (repeat (const 0)) (map f ms)
  where f _ i Perfect               = [(i  , Perfect  )]
        f m i Imperfect | i+1 <= m  = [(i+1, Imperfect)]
                        | otherwise = []

mapConsonance :: [Int] -> Constraint () Class (Int,Consonance)
mapConsonance ps = Constraint () (repeat id) (map f ps)
  where f p _ Perfect   = map ((,) ()) $ powers $ (inv perfect  ) : replicate (p-1) (inv perfect    )
        f p _ Imperfect = map ((,) ()) $ powers $ (inv imperfect) : replicate (p-1) (inv consonances)
        inv x = nub $ x ++ map recip x

filterPower :: [Int] -> Constraint Int (Int,Consonance) Consonance
filterPower ms = Constraint 0 (repeat (const 0)) (map f ms)
  where f m i (p,c) | i+p <= m  = [(i+p, c)]
                    | otherwise = []

type FilterAlteration a = Int -> [(Int,Int)] -> Constraint (Int,Int,Maybe a) a a

filterAlterationBy :: (a -> a -> Bool) -> FilterAlteration a
filterAlterationBy eq n bs = Constraint z0 (repeat $ const z0) (uncurry f <$> bs)
  where
    z0 = (n,0,Nothing)
    f lo hi (i,s,x) y
      | (s' + i') >= lo && s' <= hi = [((i', s', Just y), y)]
      | otherwise = []
      where
        s' = if maybe False (eq y) x then s else s+1
        i' = i-1

dir :: (Ord a, Num a) => a -> a -> Bool
dir x y = (x <= 1 && y <= 1) ||
          (x >= 1 && y >= 1)

toPitch :: Pitch -> Pitch -> [Int] -> Constraint (Int,Pitch) Consonance Pitch
toPitch lo hi rs = Constraint (0,1) (repeat $ const (0,1)) (map f rs)
  where
    f r (i,v) x | i == 0    = [((i+1, v'), v')]
                | i == r    = [((1  , v'), v')]
                | otherwise = [((i+1, v ), v )]
      where
        v' = clamp lo hi $ v*x

filterConsonant :: Constraint (Int, M.IntMap [Pitch]) Pitch Pitch
filterConsonant = Constraint (0,M.empty) (repeat $ \(_,mp) -> (0,mp)) (repeat f)
  where
    f (i,mp) x
      | maybe True (all (isConsonant x)) (M.lookup i mp) = [((i+1, mp'), x)]
      | otherwise = []
      where mp' = M.insertWith (++) i [x] mp
          
filterCycleBy :: (a -> a -> Bool) -> Int -> Constraint (Int, Maybe a) a a
filterCycleBy eq n = Constraint z0 (repeat $ const z0) (repeat f)
  where
    z0 = (n,Nothing)
    f (i,Nothing) x = [((i-1, Just x), x)]
    f (1,Just x ) y | eq x y    = [(z0, y)]
                    | otherwise = []
    f (i,x) y = [((i-1, x), y)]
    
powers :: [[Rational]] -> [(Int,Rational)]
powers = nub' . pow (fromInteger 1) 0
  where
    nub' = map (minimumOn fst) . groupOn snd . sortOn snd
    pow v m [] = return (m,v)
    pow v m (xs:xss) = do
      x <- xs
      if x == fromInteger 1
        then pow v m xss
        else pow (clamp 0.5 2 $ v*x) (m+1) xss

clamp :: Rational -> Rational -> Rational -> Rational
clamp lo hi x | x < lo    = x*2
              | x > hi    = x/2
              | otherwise = x


melody :: MonadInterleave m => m [[Pitch]]
melody = solve 32 4 $ mapClass
  .> filterImperfect [1,2,3,3]
  .> mapConsonance   [1,1,2,2]
  .> filterPower     [32,32,50,50]
  .> filterAlterationBy dir 32 [(2,4),(2,4),(2,8),(2,8)]  
  .> toPitch 0.5 2 [4,3,2,1]
  .> filterAlterationBy (==) 32 [(2,8),(2,8),(4,16),(8,32)]
  .> filterConsonant
  .> filterCycleBy isConsonant 32
