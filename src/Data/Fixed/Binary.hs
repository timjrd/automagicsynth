{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-|
This module defines a type for binary fixed-point arithmetic. The main
advantage this provides over decimal fixed-point arithmetic is that
the point is maintained using fast bit shifts instead of slow 'div'
operations. This type is also polymorphic on the underlying
representation, so you can use whatever size and signedness you
want.
-}
module Data.Fixed.Binary
       ( div'
       , mod'
       , divMod'
       , Fixed ()
       , SuperTypeable (..)
       , HasResolution (..)
       , E0, E1, E2, E4, E8, E10, E16, E20, E30, E32, E64
       , S, P
       , fixedRadix, fixedSize, fromRealFloat
       , (:+), (*.), (*!)
       , (:-), (/.), (/!)
       ) where

import Control.Applicative
import Control.Arrow
import Data.Bits
import Data.Fixed (div', mod', divMod')
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Typeable
import Data.Word
import Text.Read
import qualified Text.Read.Lex as L

-- | The first type parameter represents the number of bits to devote
-- to the fractional part of the number. The second type parameter is
-- the underlying representation. For example, @Fixed E8 Int16@ uses
-- eight bits for the integer component (of which one bit is used for
-- the sign) and eight bits for the fractional component.
newtype Fixed r a = Fixed { unFixed :: a }
                  deriving (Bounded, Enum, Eq, Ord, Typeable)

inFixed :: (a -> b) -> (Fixed r a -> Fixed s b)
{-# INLINE inFixed #-}
inFixed = (Fixed .) . (. unFixed)

inFixed2 :: (a -> b -> c) -> (Fixed r a -> Fixed s b -> Fixed t c)
{-# INLINE inFixed2 #-}
inFixed2 = (inFixed .) . (. unFixed)

-- | Instances of this class are useful as the first parameter of
-- 'Fixed'.
class HasResolution r where
  -- | Given a fixed-point number, give the number of bits used to
  -- represent its fractional part.
  resolution :: Num a => Fixed r a -> Int

withResolution :: (HasResolution r, Num a) => (Int -> Fixed r a) -> Fixed r a
{-# INLINE withResolution #-}
withResolution f = withType (f . resolution)
  where withType :: (Fixed r a -> Fixed r a) -> Fixed r a
        withType g = g undefined

-- Read a signed number. Stolen from GHC.Read.
readNumber :: Num a => (L.Lexeme -> ReadPrec a) -> ReadPrec a
readNumber convert =
  parens
  ( do x <- lexP
       case x of
         L.Symbol "-" -> do y <- lexP
                            n <- convert y
                            return (negate n)

         _   -> convert x
  )

-- Stolen from GHC.Read.
convertFrac :: Fractional a => L.Lexeme -> ReadPrec a
convertFrac (L.Number x) = return $ fromRational $ L.numberToRational x
convertFrac _ = pfail

instance ( HasResolution r, Bits a, Bits (Super a), Integral a
         , Integral (Super a), SuperTypeable a) => Read (Fixed r a) where
  readPrec = readNumber convertFrac

instance ( HasResolution r, Bits a, Bits (Super a), Integral a
         , Integral (Super a), SuperTypeable a) => Show (Fixed r a) where
  show (properFraction -> (i, f)) =
    show (i :: Integer) ++ "." ++ (uncurry pad . second (show . numerator) .
                                   fromJust . find ((==1) . denominator . snd) .
                                   iterate (succ *** (*10)) . (,) 0 $
                                   toRational f)
    where pad n str = replicate (n - length str) '0' ++ str

instance ( HasResolution r, Bits a, Bits (Super a), Integral a, Num (Super a)
         , Integral (Super a), SuperTypeable a) => Num (Fixed r a) where
  {-# INLINE (+) #-}
  (+) = inFixed2 (+)
  {-# INLINE (-) #-}
  (-) = inFixed2 (-)
  {-# INLINE (*) #-}
  (*) = fmap subCast . (*!) `on` superCast
  {-# INLINE negate #-}
  negate = inFixed negate
  {-# INLINE abs #-}
  abs = inFixed abs
  {-# INLINE signum #-}
  signum (Fixed x) = withResolution $ \s -> Fixed $ signum x `shiftL` s
  {-# INLINE fromInteger #-}
  fromInteger i = withResolution $ Fixed . shiftL (fromInteger i)

instance ( HasResolution r, Bits a, Bits (Super a), Integral a
         , Integral (Super a), SuperTypeable a) => Real (Fixed r a) where
  {-# INLINE toRational #-}
  toRational x = toRational (unFixed x) / toRational (2 ^ resolution x :: Integer)

instance ( HasResolution r, Bits a, Bits (Super a), Integral a
         , Integral (Super a), SuperTypeable a) => Fractional (Fixed r a) where
  {-# INLINE (/) #-}
  (/) = fmap subCast . (/!) `on` superCast
  {-# INLINE recip #-}
  recip x = Fixed . subCast $ (1 `shiftL` (2 * resolution x)) `eucQuot` superCast (unFixed x)
  {-# INLINE fromRational #-}
  fromRational r = withResolution $ \s ->
    Fixed . floor $ (numerator r `shiftL` s) % denominator r

instance ( HasResolution r, Bits a, Bits (Super a), Integral a
         , Integral (Super a), SuperTypeable a) => RealFrac (Fixed r a) where
  {-# INLINE properFraction #-}
  properFraction a = let i = truncate a in (i, a - fromIntegral i)
  {-# INLINE truncate #-}
  truncate = truncate . toRational
  {-# INLINE round #-}
  round = round . toRational
  {-# INLINE ceiling #-}
  ceiling = ceiling . toRational
  {-# INLINE floor #-}
  floor = floor . toRational

-- | Fast conversion between fixed-point numbers with the same
-- fractional size.
fixedRadix :: (Integral a, Num b) => Fixed r a -> Fixed r b
{-# INLINE fixedRadix #-}
fixedRadix = inFixed fromIntegral

-- TODO Can't I write this as one awesome, polymorphic rule?
{-# RULES
"realToFrac/fixedRadixInt"     forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Int
"realToFrac/fixedRadixInt8"    forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Int8
"realToFrac/fixedRadixInt16"   forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Int16
"realToFrac/fixedRadixInt32"   forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Int32
"realToFrac/fixedRadixInt64"   forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Int64
"realToFrac/fixedRadixWord"    forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Word
"realToFrac/fixedRadixWord8"   forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Word8
"realToFrac/fixedRadixWord16"  forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Word16
"realToFrac/fixedRadixWord32"  forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Word32
"realToFrac/fixedRadixWord64"  forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Word64
"realToFrac/fixedRadixInteger" forall (x :: Integral a => Fixed r a). realToFrac x = fixedRadix x :: Fixed r Integer
  #-}

-- | Fast conversion between fixed-point numbers with the same
-- representation size.
fixedSize :: (HasResolution r, HasResolution s, Bits a, Num a) => Fixed r a -> Fixed s a
{-# INLINE fixedSize #-}
fixedSize x = withResolution $ \s -> Fixed $ unFixed x `shift` (s - resolution x)
-- TODO Rewrite rules?

-- | Multiplication without throwing away fractional information.
(*.) :: (Num (Super a), SuperTypeable a) => Fixed r a -> Fixed s a -> Fixed (r :+ s) a
{-# INLINE (*.) #-}
(*.) = inFixed2 ((fmap subCast . (*)) `on` superCast)

-- | Division while removing unnecessary bits in the result's
-- fractional part.
(/.) :: Integral a => Fixed r a -> Fixed s a -> Fixed (r :- s) a
{-# INLINE (/.) #-}
(/.) = inFixed2 eucQuot

-- | Perform a multiplication without adding any extra bits for the
-- intermediate steps. This may be faster (especially when you are
-- already working with native-sized integer data), but it's only safe
-- to use if you are sure that the multiplication won't
-- overflow. Normal multiplication is equivalent to @\x y -> subCast
-- (superCast x *!  superCast y)@.
(*!) :: (HasResolution r, Bits a, Num a) => Fixed r a -> Fixed r a -> Fixed r a
{-# INLINE (*!) #-}
Fixed x *! Fixed y = withResolution $ Fixed . shiftR (x * y)

-- | Perform a division without adding any extra bits for the
-- intermediate steps. This may be faster if supercasting brings it up
-- to a non-native size, but you need to be sure that the shifting
-- before the division won't cause an overflow.
(/!) :: (HasResolution r, Bits a, Integral a) => Fixed r a -> Fixed r a -> Fixed r a
{-# INLINE (/!) #-}
a /! b = Fixed $ (unFixed a `shiftL` resolution a) `eucQuot` unFixed b

-- TODO Don't assume it's a binary float so that we can actually
-- expose this function.
toRealFloat :: (HasResolution r, Integral a, RealFloat b) => Fixed r a -> b
{-# INLINE toRealFloat #-}
toRealFloat = liftA2 encodeFloat (fromIntegral . unFixed) (negate . resolution)
{-# SPECIALIZE toRealFloat :: (HasResolution r, Integral a) => Fixed r a -> Float #-}
{-# SPECIALIZE toRealFloat :: (HasResolution r, Integral a) => Fixed r a -> Double #-}

{-# RULES
"realToFrac/Float"  forall (x :: (HasResolution r, Integral a) => Fixed r a). realToFrac x = toRealFloat x :: Float
"realToFrac/Double" forall (x :: (HasResolution r, Integral a) => Fixed r a). realToFrac x = toRealFloat x :: Double
  #-}

-- | Fast conversion from floating-point to fixed-point.
fromRealFloat :: (RealFloat a, HasResolution r, Num b) => a -> Fixed r b
{-# INLINE fromRealFloat #-}
fromRealFloat x = let (s,e) = decodeFloat x
                  in withResolution $ \t -> Fixed . fromIntegral $ shiftBaseExp s (floatRadix x) (t + e)
{-# SPECIALIZE fromRealFloat :: (HasResolution r, Num b) => Float -> Fixed r b #-}
{-# SPECIALIZE fromRealFloat :: (HasResolution r, Num b) => Double -> Fixed r b #-}
-- TODO Rewrite rules?

shiftBaseExp :: Integer -> Integer -> Int -> Integer
shiftBaseExp x b e | e < 0     = x `eucQuot` (b ^ negate e)
                   | otherwise = x * (b ^ e)

data E0

-- | Increment a resolution
data S n

-- | Add resolutions
type family a :+ b
type instance E0 :+ b = b
type instance S a :+ b = S (a :+ b)

-- | Subtract resolutions
type family a :- b
type instance a :- E0 = a
type instance S a :- S b = a :- b

-- | Decrement a resolution
type family P a
type instance P (S a) = a

type E1  = S E0
type E2  = E1 :+ E1
type E4  = E2 :+ E2
type E8  = E4 :+ E4
type E10 = S (S E8)
type E16 = E8 :+ E8
type E20 = E10 :+ E10
type E30 = E20 :+ E10
type E32 = E16 :+ E16
type E64 = E32 :+ E32

instance HasResolution n => HasResolution (S n) where
  {-# INLINE resolution #-}
  resolution = succ . resolution' undefined
    where resolution' :: (HasResolution n, Num a) =>
                         Fixed n a -> Fixed (S n) a -> Int
          {-# INLINE resolution' #-}
          resolution' dummy = const $ resolution dummy

instance HasResolution E0 where
  {-# INLINE resolution #-}
  resolution = const 0

-- | Instances of 'SuperTypeable' can be cast up to and down from a
-- supertype. If the type is bounded, the supertype must be able to
-- hold at least twice as much information to be a valid instance.
class SuperTypeable a where
  type Super a
  
  -- | Losslessly cast to a supertype.
  superCast :: a -> Super a
  
  -- | Cast to a subtype. Information may be lost.
  subCast :: Super a -> a

{-# RULES
"subCast . superCast" subCast . superCast = id
  #-}

instance (SuperTypeable a, Num a, Num (Super a), Integral a, Integral (Super a)) =>
         SuperTypeable (Fixed r a) where
  type Super (Fixed r a) = Fixed r (Super a)
  {-# INLINE superCast #-}
  superCast = fixedRadix
  {-# INLINE subCast #-}
  subCast = fixedRadix

instance SuperTypeable Word8 where
  type Super Word8 = Word16
  {-# INLINE superCast #-}
  superCast = fromIntegral
  {-# INLINE subCast #-}
  subCast = fromIntegral

instance SuperTypeable Word16 where
  type Super Word16 = Word32
  {-# INLINE superCast #-}
  superCast = fromIntegral
  {-# INLINE subCast #-}
  subCast = fromIntegral

instance SuperTypeable Word32 where
  type Super Word32 = Word64
  {-# INLINE superCast #-}
  superCast = fromIntegral
  {-# INLINE subCast #-}
  subCast = fromIntegral

instance SuperTypeable Word64 where
  type Super Word64 = Integer
  {-# INLINE superCast #-}
  superCast = fromIntegral
  {-# INLINE subCast #-}
  subCast = fromIntegral

instance SuperTypeable Word where
#ifdef i386_HOST_ARCH
  type Super Word = Word64
#else
  type Super Word = Integer
#endif
  {-# INLINE superCast #-}
  superCast = fromIntegral
  {-# INLINE subCast #-}
  subCast = fromIntegral

instance SuperTypeable Int8 where
  type Super Int8 = Int16
  {-# INLINE superCast #-}
  superCast = fromIntegral
  {-# INLINE subCast #-}
  subCast = fromIntegral

instance SuperTypeable Int16 where
  type Super Int16 = Int32
  {-# INLINE superCast #-}
  superCast = fromIntegral
  {-# INLINE subCast #-}
  subCast = fromIntegral

instance SuperTypeable Int32 where
  type Super Int32 = Int64
  {-# INLINE superCast #-}
  superCast = fromIntegral
  {-# INLINE subCast #-}
  subCast = fromIntegral

instance SuperTypeable Int64 where
  type Super Int64 = Integer
  {-# INLINE superCast #-}
  superCast = fromIntegral
  {-# INLINE subCast #-}
  subCast = fromIntegral

instance SuperTypeable Int where
#ifdef i386_HOST_ARCH
  type Super Int = Int64
#else
  type Super Int = Integer
#endif
  {-# INLINE superCast #-}
  superCast = fromIntegral
  {-# INLINE subCast #-}
  subCast = fromIntegral

instance SuperTypeable Integer where
  type Super Integer = Integer
  {-# INLINE superCast #-}
  superCast = id
  {-# INLINE subCast #-}
  subCast = id

eucQuot :: Integral a => a -> a -> a
{-# INLINE eucQuot #-}
a `eucQuot` b
  | a >= 0    = a `quot` b
  | b >  0    = ((a + 1) `quot` b) - 1
  | otherwise = ((a + 1) `quot` b) + 1
