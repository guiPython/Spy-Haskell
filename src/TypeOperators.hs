{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeOperators where

import Data.Kind ( Type, Constraint )
import Data.Type.Bool (If)
import Spies (SpySpecifications)

type Elem :: a -> [a] -> Bool
type family Elem x xs where
    Elem _ '[] = 'False
    Elem x (x ': _)  = 'True
    Elem x (_ ': xs) = Elem x xs

type Contain :: [SpySpecifications] -> [SpySpecifications] -> Constraint
type family Contain specifications other where
    Contain specifications other = In specifications other ~ 'True

type In :: [a] -> [a] -> Bool
type family In items other where
    In '[] _ = 'True
    In _ '[] = 'False
    In (x ': xs) ys = If (Elem x ys) (In xs ys) 'False

type (#<>) :: [a] -> [a] -> [a]
type family xs #<> ys where
    '[] #<> '[] = '[]
    xs  #<> '[] =  xs
    '[] #<>  ys =  ys
    (x ': xs) #<> ys =  xs #<> (x ': ys)