module Constraint where

import Data.List
import Data.Either
import Data.Maybe
import Data.Functor.Identity

import Control.Monad.Random.Lazy

import System.Random.Shuffle

import Util

data Constraint  z a b = Constraint z [z -> z] [ConstraintF z a b]
type ConstraintF z a b = z -> a -> [(z,b)]

infixl 9 .>
(.>) :: Constraint y a b -> Constraint z b c -> Constraint (y,z) a c
(Constraint y ys f1) .> (Constraint z zs f2) =
  Constraint (y,z) (zipWith zipF ys zs) (zipWith composeF f1 f2)

composeF :: ConstraintF y a b -> ConstraintF z b c -> ConstraintF (y,z) a c
composeF f1 f2 (y,z) a = concatMap sub (f1 y a)
  where sub (y',b) = map (\(z',c) -> ((y',z'),c)) $ f2 z b

zipF :: (y -> y) -> (z -> z) -> (y,z) -> (y,z)
zipF f1 f2 (y,z) = (f1 y, f2 z)

idF :: ConstraintF z a a
idF z a = [(z,a)]

cid :: Constraint z a a
cid = Constraint undefined (repeat id) (repeat idF)

errorZs = error "solve: not enough accumulator setters"
errorFs = error "solve: not enough constraint functions"

-- solve :: Constraint z a b -> [[a]] -> [ [[b]] ]
-- solve (Constraint _ [] _ ) = errorZs
-- solve (Constraint z zs fs) = f ((head zs) z) (tail zs) fs
--   where
--     f _ _  _  [] = [ [] ]
--     f _ [] _  _  = errorZs
--     f _ _  [] _  = errorFs
--     f z zs fs (    []:xss) = map ([]:) $ f ((head zs) z) (tail zs) (tail fs) xss
--     f z zs fs ((x:xs):xss) = concatMap sub $ (head fs) z x
--       where sub (z',y) = map (cons y) $ f z' zs fs (xs:xss)
--             cons y (xs':xss') = (y:xs'):xss'

type Shuffle    m z b = [(z,b)] -> m [(z,b)]
type Interleave m b   = m (Either Int [ [[b]] ]) -> m (Either Int [ [[b]] ])

solveM :: Monad m => Shuffle m z b -> Interleave m b
       -> Int -> [[a]] -> Constraint z a b -> m [ [[b]] ]
solveM _  _  _  _  (Constraint _ [] _ ) = errorZs
solveM sf il it xs (Constraint z zs fs) = either (const []) id <$> f ((head zs) z) (tail zs) fs xs
  where
    f _ _  _  [] = return $ Right [ [] ]
    -- f _ [] _  _  = errorZs
    f _ _  [] _  = errorFs
    f z zs fs (    []:xss) = (fmap $ map ([]:)) <$> f ((head zs) z) (tail zs) (tail fs) xss
    f z zs fs ((x:xs):xss) = il $ do
      ys <- sf $ (head fs) z x
      if null ys
        then return $ Left 1
        else do
          subs <- mapM sub ys
          return $ concatIt 0 subs
      where
        sub (z',y) = (fmap $ map $ cons y) <$> f z' zs fs (xs:xss)
        cons y (xs':xss') = (y:xs'):xss'

        concatIt i          []  = Left i
        concatIt i (Right x:xs) = Right $ x ++ either (const []) id (concatIt 0 xs)
        concatIt i (Left  x:xs)
          | i+x > it  = Left (i+x)
          | otherwise = concatIt (i+x) xs

solve :: MonadInterleave m => Int -> Int -> Constraint z () b -> m (Maybe [[b]])
solve n m c = -- maybe (error "solve: too hard") id <$>
  fmap fst <$> uncons <$> concatMapM f [10, 15 .. 500]
  where f it = solveM shuffleM interleave it (replicate m $ replicate n ()) c
