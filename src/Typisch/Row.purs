module Typisch.Row
  ( class Lacks
  , class Cons
  , class Cons2
  , class Changes
  , class Replaces
  , module Prim.Row
  ) where

import Prim.Row as Row
import Prim.Row (class Nub, class Union)
import Data.Symbol (class IsSymbol)

-- | A version of `Prim.Row.Lacks` that infers `Data.Symbol.IsSymbol`
-- | for you to ease record modification.
class
  ( IsSymbol label
  , Row.Lacks label row
  ) <= Lacks (label :: Symbol) (row :: # Type)

instance lacks ::
  ( IsSymbol label
  , Row.Lacks label row
  ) => Lacks label row

-- | A version of `Prim.Row.Cons` that infers `Data.Symbol.IsSymbol`
-- | and `Prim.Row.Lacks` for you to ease record modification.
class
  ( Lacks label tail
  , Row.Cons label a tail row
  ) <= Cons
       (label :: Symbol)
       (a     :: Type)
       (tail  :: # Type)
       (row   :: # Type)
       | label a tail -> row
       , label row -> a tail

instance cons ::
  ( Lacks label tail
  , Row.Cons label a tail row
  ) => Cons label a tail row

-- | `Cons`, but for two items.
class
  ( Lacks labelB base
  , Cons labelA a base rowA
  , Cons labelB b rowA rowAB
  ) <= Cons2
       (labelA :: Symbol)
       (a      :: Type)
       (labelB :: Symbol)
       (b      :: Type)
       (base   :: # Type)
       (rowA   :: # Type) -- base + a
       (rowAB  :: # Type) -- base + a + b
       | labelA a base -> rowA
       , labelB b rowA -> rowAB
       , rowAB b labelB -> rowA
       , rowA labelA a -> base

instance cons2 ::
  ( Lacks labelB base
  , Cons labelA a base rowA
  , Cons labelB b rowA rowAB
  ) => Cons2 labelA a labelB b base rowA rowAB

-- | A class that represents changing the type of a label in a row.
class
  ( Cons label a tail rowA
  , Cons label b tail rowB
  ) <= Changes
       (label :: Symbol)
       (a     :: Type)
       (b     :: Type)
       (tail  :: # Type)
       (rowA  :: # Type)
       (rowB  :: # Type)
       | label a tail -> rowA
       , label b tail -> rowB
       , label rowA -> a tail
       , label rowB -> b tail

instance changes ::
  ( Cons label a tail rowA
  , Cons label b tail rowB
  ) => Changes label a b tail rowA rowB

-- | A class that represents replacing a row entry with another one.
class
  ( Cons labelA a tail rowA
  , Cons labelB b tail rowB
  ) <= Replaces
       (labelA :: Symbol)
       (a      :: Type)
       (labelB :: Symbol)
       (b      :: Type)
       (tail   :: # Type)
       (rowA   :: # Type)
       (rowB   :: # Type)
       | labelA a tail -> rowA
       , labelB b tail -> rowB
       , labelA rowA -> a tail
       , labelB rowB -> b tail

instance replaces ::
  ( Cons labelA a tail rowA
  , Cons labelB b tail rowB
  ) => Replaces labelA a labelB b tail rowA rowB
