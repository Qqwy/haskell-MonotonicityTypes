{-# LANGUAGE
    DataKinds,
    KindSignatures,
    AllowAmbiguousTypes,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    UndecidableInstances,
    TypeApplications
  #-}

module MonotonicityTypes where

import Control.Category hiding ((.), id)
import Control.Arrow
import Data.Kind (Type)

{-| Tonicity Qualifiers from section 4.2 of the paper.-}
data TonicityQualifier = Constant | Monotone | Antitone | Unknown | Discarded

newtype Sfun (t :: TonicityQualifier) input output = UnsafeMakeSfun { applySfun :: input -> output }

-- The composition of two Sfuns is the composition of their two functions,
-- but with the type restricted to the composition of their tonicity qualifiers.
composeSfuns :: Sfun t1 b c -> Sfun t2 a b -> Sfun (ComposeTonicity t1 t2) a c
composeSfuns (UnsafeMakeSfun f) (UnsafeMakeSfun g) = UnsafeMakeSfun (f . g)

type family ComposeTonicity (t1 :: TonicityQualifier) (t2 :: TonicityQualifier)  :: TonicityQualifier where
  ComposeTonicity Monotone Monotone = Monotone
  ComposeTonicity Monotone Antitone = Antitone
  ComposeTonicity Antitone Antitone = Monotone
  ComposeTonicity Antitone Monotone = Antitone
  ComposeTonicity Constant other = other
  ComposeTonicity other Constant = other
  ComposeTonicity Discarded _ = Discarded
  ComposeTonicity _ Discarded = Discarded
  ComposeTonicity _ _ = Unknown
