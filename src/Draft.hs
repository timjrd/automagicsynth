-- /!\ THIS MODULE IS FOR DEAD-CODE ARCHIVING ONLY /!\
module Draft where

import Data.Ratio
import Data.Tree
import Graphics.Gnuplot.Simple
import System.IO
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffleM)
import Data.List (transpose, uncons)
import Control.Concurrent (threadDelay)
import Control.Monad.Random.Lazy
import qualified Data.Set as S

import Data.Maybe (catMaybes)
import Data.Fixed (mod')
import Data.Function ((&))
import Control.Arrow ((>>>))
import Data.Sequence (Seq((:<|), (:|>)))
import qualified Data.Sequence as SQ

import Data.Map ((!))

import qualified CompositionDraft as C
import qualified Util
import qualified Tone
import qualified Envelope

la   = 440
do'  = 554.37
re   = 587.33
mi   = 659.26
fa   = 698.46
sol  = 783.99
la2  = 880
la2' = 932.33

frameRate :: Integral a => a
frameRate = 44100

frame :: Fractional a => a
frame = 1 / fromIntegral frameRate

-- zero = 2 / (2^16)

-- main :: IO ()
-- main = do

--   -- let notes = [ la, re, la, mi, la, fa, la, re, la, mi, la, fa, la,
--   --               sol, la, mi, la, fa, la, sol, la, la2, la, fa, la, sol, la, la2,
--   --               la, la2', la, sol, la, la2, la, fa, la, sol, la, mi, la, fa, la,
--   --               re, la, mi, la, do', la, re ]

--   -- let notes = [ re , mi , fa  , re
--   --             , mi , fa , sol , mi
--   --             , fa , sol, la2 , fa
--   --             , sol, la2, la2', sol
--   --             , la2, fa , sol , mi
--   --             , fa , re , mi  , do'
--   --             , re, 1 ]

--   -- let notes = [ sol, la2, la2' ]
  
--   patches1 <- repeat <$> getRandom
--   patches2 <- repeat <$> getRandom
--   patches3 <- repeat <$> getRandom

--   -- r1 <- getRandomRs (0,3)
--   -- r2 <- getRandomRs (0,3)
--   -- r3 <- getRandomRs (0,3)

--   -- rs  <- getRandomRs (0.5,2)
--   -- hzs <- getRandomRs (la/2,la2'/2)

--   -- patches <- getRandoms

--   -- r <- getRandomR (0,1000)
--   -- is <- getRandomRs (0, length consonances - 1)
--   -- let hzs = map ((*hz) . fromRational . (consonances!!)) is
--   -- cc <- shuffleM $ take 5000 $ consonantCycles!!16
--   -- let hzs = map ((*hz) . fromRational) $ concat $ concatMap (replicate 1) cc
--   let hzs = repeat $ 440/2

--   cc <- evalRandIO $ C.consonantCycles''' 16 0.99 [2,2,2]
--   let chords' = concat cc
  
--   let voices = concatMap f $ zip3 chords' hzs $ transpose [patches1, patches2, patches3]
--         where
--           f (chord, hz, patches) = map mix' $ transpose $ map (uncurry voice) $ zip chord patches
--             where
--               mix' xs = sum xs / dup (fromIntegral $ length xs)
--               voice c patch = sampleR 0 0.125 $ \t -> dup (adsr 0.01 0.015 0.6 0.05 0.125 t) * tone patch (fromRational c * hz) t

--   -- let samples = map (\(a,b) -> (a + b) / dup 2) $ zip a b
--   --       where a = ferp 0 1.3 $ concatMap (flip map notes) $ map tone patches
--   --             b = ferp 0 1.3 $ concatMap (flip map notes) $ map (\(p,c) -> tone p . (*c)) $ zip patches $ map fromRational consonances
  
--   -- let samples = map (\(t,x) -> let up = min 1 (t/2); down = min 1 ((60*2-t)/2) in dup (up*down) * x )
--   --               $ zip [0,frame..] $ take (frameRate*60*2) $ map (\(a,b,c) -> (a+b+c) / dup 3) $ zip3 a b c
--   --       where a = ferp 0 4 $ map (uncurry tone) $ zip patches1' notes1
--   --             b = ferp 0 6 $ map (uncurry tone) $ zip patches2' notes2
--   --             c = ferp 0 8 $ map (uncurry tone) $ zip patches3' notes3              
--   --             patches1' = concatMap (replicate 4) patches1
--   --             patches2' = concatMap (replicate 3) patches2
--   --             patches3' = concatMap (replicate 2) patches3
--   --             notes1 = map (*hz)     $ map (fromRational . (chords!!2!!3!!)) r1
--   --             notes2 = map (*hz)     $ map (fromRational . (chords!!2!!3!!)) r2
--   --             notes3 = map (*(hz/2)) $ map (fromRational . (chords!!2!!3!!)) r3

--   -- samples <- snare 0.5

--   -- let samples = ferp 0 0.3 $ map f hzs
--   --       where f (hz1,hz2) t = dup $ (sine hz1 t + sine hz2 t) / 2
--   --             hzs = map (\(r,hz) -> (hz, r*hz)) $ concatMap (uncurry zip) $ zip (map repeat consonances) $ repeat notes

--   -- hPutStrLn stderr $ show $ take 20 $ zip rsn rsd

--   -- drumL <- drum 1000 0.5
--   -- drumR <- drum 1000 0.5
  
--   -- let samples = cycle $ zip drumL drumR

--   snare' <- map (*0.7)
--     <$> cycle
--     <$> snare 0.2

--   let drum' = map (*2) $ zipWith mix' (sampleR 0 0.25 (const 0) ++ snare') $ cycle $ kick 0.5

--   let (samplesL,samplesR) = unzip $ voices -- zipWith mix' voices $ map dup drum'
--       samples = zipWith (*) (map dup $ sample $ \t -> ramp 0 0.1 0 1 t)
--         $ stretch $ zip (delay 0.2 0.11 samplesL) (delay 0.2 0.13 samplesR)

--   plotList [] $ map (adsr 1 1 0.5 1 1) [0,frame..10]
--   threadDelay $ 1000 * 60 * 60

--   -- hPutWAVE stdout (WAVE headers (map (\(l,r) -> [doubleToSample (-l), doubleToSample (-r)]) samples))

sampleR :: (Fractional a, Enum a) => a -> a -> (a -> b) -> [b]
sampleR t0 tn f = map f [t0, t0+frame .. tn-frame]

sample :: (Fractional a, Enum a) => (a -> b) -> [b]
sample f = map f [0,frame..]

env = adsr 6 a d 0.8 r (1-a-d-r)
  where a = 0.1
        d = 0.2
        r = 0.3

dup :: Double -> (Double,Double)
dup x = (x,x)

ferp :: Double -> Double -> [Double -> (Double,Double)] -> [(Double,Double)]
ferp _  _  []     = []
ferp _  _  (_:[]) = []
ferp t0 dt (f1:f2:fs) = sampleR t0 (t0+dt) f ++ ferp (t0+dt) dt (f2:fs)
  where
    f t =
      -- dup (env k) * f1 t
      dup (1-k) * f1 t + dup k * f2 t
      where
        k = (t-t0)/dt

at :: Double -> Double -> Double -> Double -> (Double -> Double) -> (Double -> Double)
at x0 y0 x1 y1 f x = y0 + (y1 - y0) * f ((x - x0) / (x1 - x0))

gis :: Double -> (Double -> Double)
gis k = (1-) . (sig k)

sig :: Double -> (Double -> Double)
sig k x =
  -- if x <= 0 then 0
  -- else if x >= 1 then 1
  -- else
  ky * (dy + ff (dx + k * x))
  where dx = 0.5 - k * 0.5
        dy = - ff dx
        ky = 1 / (dy + ff (dx + k))
        
        ff x = 0.5 + f (x - 0.5) / 2
        f    = tanh
        -- for f, see:
        -- https://en.wikipedia.org/wiki/Sigmoid_function#/media/File:Gjl-t(x).svg

ramp fromX toX fromY toY t = max (min fromY toY) $ min (max fromY toY)
                             $ fromY + ((t-fromX) / (toX-fromX)) * (toY-fromY)
                             
sine hz t = sin (2*pi*hz*t)
triangle hz t = f $ (hz*t) `mod'` 1
  where f t = ramp 0     (1/4) 0    1    t
            + ramp (1/4) (3/4) 1    (-1) t
            + ramp (3/4) 1     (-1) 0    t


sine' a d hz t = a * sin (2 * pi * hz * t + d)

add :: [Double] -> Double -> Double -> (Double -> Double)
add as d hz t = [1..]
  & map (*hz) 
  & zip as
  & map (\(a,hz) -> sine' a d hz t)
  & sum
  & (/ sum as)

fm :: Double -> Double -> Double -> (Double -> Double)
fm hzc hzm im t = sin (2*pi * hzc * t + im * sin (2*pi * hzm * t)) 

data Patch = Patch
  { modulatingToneFactor       :: Int
  , modulatingToneShift        :: Double
  , stereoModulatingToneShift  :: Double
  , modulationIndex            :: Double
  , stereoModulationIndexShift :: Double }

loPatch = Patch
  { modulatingToneFactor       = 1
  , modulatingToneShift        = -7
  , stereoModulatingToneShift  = -5
  , modulationIndex            = -1
  , stereoModulationIndexShift = -1 }

hiPatch = Patch
  { modulatingToneFactor       = 5
  , modulatingToneShift        = 7
  , stereoModulatingToneShift  = 5
  , modulationIndex            = 1
  , stereoModulationIndexShift = 1 }

instance Random Patch where
  random = randomR (loPatch,hiPatch)
  
  randomR ( (Patch m1 dm1 ddm1 im1 dim1)
          , (Patch m2 dm2 ddm2 im2 dim2) ) = runRand $ do
    m   <- getRandomR (m1  , m2  )
    dm  <- getRandomR (dm1 , dm2 )
    ddm <- getRandomR (ddm1, ddm2)
    im  <- getRandomR (im1 , im2 )
    dim <- getRandomR (dim1, dim2)
    return $ Patch m dm ddm im dim

mix :: Double -> Double -> Double -> Double -> (Double,Double)
mix panA panB a b = ( (left  + center) // (left1  + center1)
                    , (right + center) // (right1 + center1) )
  where
    left    = nabs panA * a + nabs panB * b
    left1   = nabs panA + nabs panB
    right   = pabs panA * a + pabs panB * b
    right1  = pabs panA + pabs panB
    center  = (1 - abs panA) * a + (1 - abs panB) * b
    center1 = (1 - abs panA) + (1 - abs panB)
    nabs x  = abs (min 0 x)
    pabs x  = abs (max 0 x)
    a // b  = if b == 0 then 0 else a / b

tone :: Patch -> Double -> (Double -> (Double,Double))
tone (Patch m dm ddm im dim) hz t = mix (-0.5) 0.5 left right
  where left  = fm hz (hz * fromIntegral m + dm + ddm) (im + dim) t
        right = fm hz (hz * fromIntegral m + dm - ddm) (im - dim) t
        
-- adsr :: Double -> Double -> Double -> Double -> Double -> Double
--      -> (Double -> Double)
-- adsr k a d s r dt t = if t <= 0 || t >= (a+d+dt+r)
--                       then 0
--                       else f t - f 0
--   where f t = at 0        0 a          1 (sig k) t
--             + at a        s (a+d)      1 (gis k) t
--             + at (a+d+dt) 0 (a+d+dt+r) s (gis k) t

adsr :: Double -> Double -> Double -> Double -> Double -> (Double -> Double)
adsr a d s r dt t = f t - f 0
  where f t = ramp 0         a           0 1 t
            + ramp a         (a+d)       1 s t
            + ramp (a+d+dt') (a+d+dt'+r) s 0 t
        dt' = max 0 $ dt-a-d-r

consonances :: [Rational]
consonances = [1%1, 1%2, 2%3, 3%4, 3%5, 4%5, 5%6, 5%8]

isConsonant :: Rational -> Rational -> Bool
isConsonant a b = or $ map (\x -> x == a/b || x == b/a) consonances

chords :: [[[Rational]]]
chords = deduplicate $ [] : f [[]]
  where
    deduplicate = map $ (map S.toDescList) . S.toDescList . S.fromList . (map S.fromList)
    f n_1 = let n = g n_1
            in if null n
               then []
               else n : f n
    g n_1 = [ x:xs | x  <- consonances
                   , xs <- n_1
                   , x `notElem` xs
                   , and $ map (isConsonant x) xs ]

-- consonantCycles :: [[[Rational]]]
-- consonantCycles = evalRand (([]:) <$> mapM enumerate [1..]) (mkStdGen 42)
--   where
--     node label = interleave $ do
--       next <- shuffleM all
--       children <- mapM node next
--       return $ Node label children
--       where
--         up   = map (label/) consonances
--         down = map (label*) consonances
--         all  = filter (label/=) $ up ++ down
    
--     enumerate n = interleave $ f n <$> node 1
--       where
--         f 0 (Node 1 _) = [[]]
--         f 0 (Node _ _) = []
--         f n (Node label children) = map (label:) $ concatMap (f $ n-1) children


consonantCycles :: (MonadRandom m, MonadInterleave m) => Int -> m [[Rational]]
consonantCycles n = interleave $
  fmap catMaybes $ sequence $ repeat $ f 1 n
  where
    f 1 0 = return $ Just []
    f _ 0 = return Nothing
    f x m = do
      c <- (consonances!!) <$> getRandomR (0, length consonances - 1)
      b <- getRandom
      let x' = if b then (x*c) else (x/c)
      if x /= x'
        then fmap (x:) <$> f x' (m-1)
        else return Nothing
      
-- noise :: MonadRandom m => m [(Double,Double)]
-- noise = do
--   left  <- getRandomRs (-1,1)
--   right <- getRandomRs (-1,1)
--   return $ zip left right

-- snare :: MonadRandom m => Double -> m [(Double,Double)]
-- snare dt = do
--   noise' <- noise
--   return $ map (uncurry (*)) $ zip noise' $ sample 0 dt $ dup . envelope
--   where envelope = adsr 8 0.03 (dt-0.05) 0 0 0


-- drum t = (triangles t + sines t) / 2
--   where hz1 = 111 + 175
--         hz2 = 111 + 224
--         hz3 = 330
--         hz4 = 180

--         triangles t = (triangle hz1 t + triangle hz2 t) / 2 * envelope 0.3 t
--         sines     t = (sine hz3 t + sine hz4 t) / 2 * envelope 0.2 t

--         envelope r = adsr 1 0.01 r 0 0 0

drum :: MonadRandom m => Int -> Double -> m [Double]
drum p b = do
  wavetable <- take p <$> getRandomRs (-1,1)
  -- let wavetable = map (sine 1) [0, (1 / fromIntegral p) .. 1]
  f (SQ.fromList wavetable) 1
  where
    k    = 200
    zero = 1/2000
    f (x1 :<| x2 :<| xs) a = do
      r <- getRandomR (0,1)
      let v = if r < b
              then  0.5 * (x1+x2)
              else -0.5 * (x1+x2)
      next <- if a > zero
              then f ((x2 :<| xs) :|> v)
                   $ (a * k + abs v) / (k+1)
              else return []
      return $ v : next

highPass :: Floating a => a -> [a] -> [a]
highPass hz = f 0
  where
    a = 1 / (2*pi * frame * hz + 1)
    f y_1 (x_1:x:xs) = y : f y (x:xs)
      where
        y = a * (y_1 + x - x_1)

sineS :: (Double -> Double) -> [Double]
sineS hz = f 0 0
  where f t a = sin a : f (t+frame) (a + 2*pi * frame * hz t)

kick :: Double -> [Double]
kick dt = zipWith (*) (sampleR 0 dt $ adsr a (dt-a) 0 0 0) $ sineS $ ramp 0 dt 80 10
  where a = 0

mix' a b = (a+b)/2

splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = x : splitEvery n xs'
  where (x,xs') = splitAt n xs

delay :: Double -> Double -> [Double] -> [Double]
delay decay delay' xs = concat $ init' : f init' tail'
  where
    (init':tail') = splitEvery (floor $ delay' * fromIntegral frameRate) xs
    f xs_1 (xs:xss) = xs' : f xs' xss
      where xs' = zipWith (\x_1 x -> x_1 * decay + x) xs_1 xs

-- reverb :: (MonadRandom m) => Double -> [Double] -> m [Double]
-- reverb delay' xs = do
--   noise <- getRandomRs (-1,1)
--   let decay  = sampleR 0 delay' $ ramp 0 delay' 1 0
--       noise' = zipWith (*) noise decay
--       f _      []     = []
--       f delays (x:xs) = 0.5 * sum inits' + x : f (map (*x) noise' : delays') xs
--         where (inits',delays') = delays & map uncons & catMaybes & unzip
--   return $ f [] xs

snare :: MonadRandom m => Double -> m [Double]
snare dt = do
  noise <- getRandomRs (-1,1)
  let sine'    = zipWith (*) sineEnv  $ sineS $ ramp 0 dt 400 370
      noise'   = zipWith (*) noiseEnv $ highPass 1000 noise
      result   = zipWith (*) env $ zipWith mix' sine' noise'
      sineEnv  = sample $ ramp 0 dt 1   0.2
      noiseEnv = sample $ ramp 0 dt 0.2 1
      env      = sampleR 0 1 $ adsr 0 dt 0 0 0
  return result

stretch :: [a] -> [a]
stretch = g 0 . concat . f . zip (sample $ \t -> max 1 $ 1/(t*18+1) * 5)
  where
    f ((k,x):xs) = replicate n (k / fromIntegral n, x) : f xs
      where n = floor k
    g sk ((k,x):xs) = if sk' >= 1 then x:x:ys else x:ys
      where sk' = sk + (k - 1)
            ys  = g (sk' `mod'` 1) xs
