module Typisch.Row
  ( class Cons
  , class Cons2
  , class Changes
  , class Replaces
  ) where

import Prim.Row as Row

class
  ( Row.Lacks label tail
  , Row.Cons label a tail row
  ) <= Cons
       (label :: Symbol)
       (a :: Type)
       (tail :: # Type)
       (row :: # Type)
       | label a tail -> row
       , label row -> a tail

instance cons ::
  ( Row.Lacks label tail
  , Row.Cons label a tail row
  ) => Cons label a tail row

class
  ( Row.Lacks labelB base
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
  ( Row.Lacks labelB base
  , Cons labelA a base rowA
  , Cons labelB b rowA rowAB
  ) => Cons2 labelA a labelB b base rowA rowAB

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
