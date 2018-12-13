module Script where

import Data.List

import Util
import Tone
import Envelope

type Duration = Rational
type Velocity = Rational
type Pitch    = Rational

data Script = Seq Script Script
            | Par Script Script
            | Empty
            | Note Duration Velocity Pitch
            -- | Rest Duration
  deriving Show

mergeNotes :: Duration -> Velocity -> [Pitch] -> Script
mergeNotes dt v = line . map f . group
  where f x = Note (dt * fromIntegral (length x)) v (head x)

line :: [Script] -> Script
line []     = Empty
line (x:xs) = Seq x $ line xs

together :: [Script] -> Script
together []     = Empty
together (x:xs) = Par x $ together xs

fromPitches :: Duration -> Velocity -> [Pitch] -> Script
fromPitches dt v = line . map (Note dt v)

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

synthScript' (Note dt v hz) = 
  withEnvelope (Envelope 0.04 0.7 0.5 0.1 4) (fromRational dt)
  $ map (dup (fromRational v) *)
  $ tone someTone (fromRational hz)
