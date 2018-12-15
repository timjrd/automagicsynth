module Script where

import Data.List

import Util
import Noise
import Tone
import Envelope

type Duration = Rational
type Velocity = Rational
type Pitch    = Rational

data Script = Seq Script Script
            | Par Script Script
            | Empty
            | Note Tone Duration Velocity Pitch
            | Rest Duration
  -- deriving Show

mergeNotes :: Tone -> Duration -> Velocity -> [Pitch] -> Script
mergeNotes tn dt v = line . map f . group
  where f x = Note tn (dt * fromIntegral (length x)) v (head x)

line :: [Script] -> Script
line []     = Empty
line (x:xs) = Seq x $ line xs

together :: [Script] -> Script
together []     = Empty
together (x:xs) = Par x $ together xs

fromPitches :: Tone -> Duration -> Velocity -> [Pitch] -> Script
fromPitches tn dt v = line . map (Note tn dt v)

synthScript :: Script -> [(Sample,Sample)]
synthScript = map fromSplit . synthScript'

synthScript' :: Script -> [Split (Sample,Sample)]

synthScript' Empty = []

synthScript' (Seq a b) = f (synthScript' a) (synthScript' b)
  where
    f (Fst x:xs)        ys  = Fst x     : f xs ys
    f (Snd x:xs) (Fst y:ys) = Fst (x+y) : f xs ys
    f (Snd x:xs) (Snd y:ys) = Snd (x+y) : f xs ys
    f        xs         []  = xs
    f        []         ys  = ys
    
synthScript' (Par a b) = f (synthScript' a) (synthScript' b)
  where
    f (Fst x:xs) (Fst y:ys) = Fst (x+y) : f xs ys
    f (Fst x:xs) (Snd y:ys) = Fst (x+y) : f xs ys
    f (Snd x:xs) (Fst y:ys) = Fst (x+y) : f xs ys
    f (Snd x:xs) (Snd y:ys) = Snd (x+y) : f xs ys
    f        xs         []  = xs
    f        []         ys  = ys

synthScript' (Note tn dt v hz) = 
  withEnvelope (Envelope 0.006 0.06 0.45 0.3) (fromRational dt)
  $ map (dup (fromRational v) *)
  $ tone tn (fromRational hz)

synthScript' (Rest dt) = replicate (samples dt) $ Fst (0,0)
