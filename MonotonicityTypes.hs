{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE GADTs  #-}

module MonotonicityTypes where

import qualified Prelude
import Prelude hiding (id, (.))
import qualified Control.Category
import qualified Control.Arrow

{-| Tonicity Qualifiers from section 4.2 of the paper.-}
data TonicityQualifier = Constant | Monotone | Antitone | Unknown | Discarded

newtype Sfun (t :: TonicityQualifier) input output = UnsafeMakeSfun { applySfun :: input -> output }

-- A few examples:
incrementSfun :: Num a => Sfun 'Monotone a a
incrementSfun = UnsafeMakeSfun (+ 1)

negateSfun :: Num a => Sfun 'Antitone a a
negateSfun = UnsafeMakeSfun (* (-1))

-- The composition of two Sfuns is the composition of their two functions,
-- but with the type restricted to the composition of their tonicity qualifiers.
composeSfuns :: Sfun t1 b c -> Sfun t2 a b -> Sfun (ComposeTonicity t1 t2) a c
composeSfuns (UnsafeMakeSfun f) (UnsafeMakeSfun g) = UnsafeMakeSfun (f Prelude.. g)

type family ComposeTonicity (t1 :: TonicityQualifier) (t2 :: TonicityQualifier)  :: TonicityQualifier where
  ComposeTonicity 'Monotone 'Monotone = 'Monotone
  ComposeTonicity 'Monotone 'Antitone = 'Antitone
  ComposeTonicity 'Antitone 'Antitone = 'Monotone
  ComposeTonicity 'Antitone 'Monotone = 'Antitone
  ComposeTonicity 'Constant other = other
  ComposeTonicity other 'Constant = other
  ComposeTonicity 'Discarded _ = 'Discarded
  ComposeTonicity _ 'Discarded = 'Discarded
  ComposeTonicity _ _ = 'Unknown

-- result1 :: Num a => Sfun 'Monotone a a
result1 = composeSfuns incrementSfun incrementSfun

-- result2 :: Num a => Sfun 'Antitone a a
result2 = composeSfuns negateSfun incrementSfun

-- result3 :: Num a => Sfun 'Antitone a a
result3 = composeSfuns incrementSfun negateSfun

-- result4 :: Num a => Sfun 'Monotone a a
result4 = composeSfuns negateSfun negateSfun

-- | Behaves to `Control.Category` but has an extra `index` kind parameter,
-- | whose type-value might change under composition.
class IndexedCategory (ic :: index -> * -> * -> *) where
  type ComposeIndexes (index1 :: index) (index2 :: index) :: index -- ^ The resulting type under composition
  id :: ic (index' :: index) a a
  (.) :: ic i1 b c -> ic i2 a b -> ic (ComposeIndexes i1 i2) a c

infixr 1 >>>
-- | Common composition function
(>>>) :: IndexedCategory ic => ic i1 a b -> ic i2 b c -> ic (ComposeIndexes i2 i1) a c
a >>> b = b . a

infixr 1 <<<
(<<<) :: IndexedCategory ic => ic i1 b c -> ic i2 a b -> ic (ComposeIndexes i1 i2) a c
(<<<) = flip (>>>)

class (IndexedCategory ic) => IndexedArrow ic where
  type DefaultIndex ic :: index
  arr :: (index ~ DefaultIndex ic) => (a -> b) -> ic index a b
  first :: ic index b c -> ic index (b, d) (c, d)

-- | Wrapper to lift normal categories to 'indexed' categories.
-- | We use the singleton kind `()` as 'index'.
newtype FreeIndexed (c :: * -> * -> *) (single :: ()) a b = FreeIndexed { getCategory :: c a b }


instance Control.Category.Category c => IndexedCategory (FreeIndexed c) where
  type ComposeIndexes a b = '()
  id = FreeIndexed Control.Category.id
  (FreeIndexed a) . (FreeIndexed b) = FreeIndexed (a Control.Category.. b)

instance Control.Arrow.Arrow c => IndexedArrow (FreeIndexed c) where
  type DefaultIndex (FreeIndexed c) = '()
  arr fun = FreeIndexed (Control.Arrow.arr fun)
  first (FreeIndexed cat) = FreeIndexed (Control.Arrow.first cat)
