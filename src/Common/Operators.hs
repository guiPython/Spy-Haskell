{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}


module Common.Operators where

import Data.Type.Bool (type (&&), type (||), Not, If)
import Data.Bool (Bool(True, False))
import Data.Kind (Constraint, Type)
import Data.Data (Proxy (Proxy))
import Data.Type.Equality (type (==))

eval :: Maybe  a -> Bool
eval (Just _) = True
eval Nothing  = False

type Elem :: a -> [a] -> Bool
type family Elem x xs where
    Elem _ '[] = 'False
    Elem x (x ': _)  = 'True
    Elem x (_ ': xs) = Elem x xs

type In :: [a] -> [a] -> Bool
type family In items other where
    In '[] _ = 'True
    In _ '[] = 'False
    In (x ': xs) ys = If (Elem x ys) (In xs ys) 'False

type Same :: [a] -> [a] -> Bool
type family Same xs ys where
  Same '[] '[] = True
  Same _ '[] = False
  Same '[] _ = False
  Same (x ': xs) (y ': ys) = If (x == y) (True && Same xs ys) False

type (#<>) :: [a] -> [a] -> [a]
type family (#<>) xs ys where
    (#<>) '[] '[] = '[]
    (#<>) xs '[] =  xs
    (#<>) '[] ys =  ys
    (#<>) (x ': xs) ys = xs #<> (x ': ys)

data Expression a =
    ETrue | EFalse |
    Spec a |
    AndExpresion  (Expression a) (Expression a) |
    OrExpression  (Expression a) (Expression a) |
    NotExpression (Expression a)

type And :: Expression a -> Expression a -> Expression a
type family And specification other where
    And specification ETrue  = AndExpresion specification ETrue
    And specification EFalse = EFalse
    And specification other = AndExpresion specification other

type Or :: Expression a -> Expression a -> Expression a
type family Or specification other where
    Or specification ETrue  = ETrue
    Or specification EFalse = OrExpression specification EFalse
    Or specification other = OrExpression specification other

type NoT :: Expression a -> Expression a
type family NoT specification where
    NoT ETrue  = EFalse
    NoT EFalse = ETrue
    NoT specification = NotExpression specification

type Eval :: Expression a -> [a] -> Bool
type family Eval expression specifications where
    Eval ETrue  _ = 'True
    Eval EFalse _ = 'False
    Eval (Spec a) specs = Elem a specs
    Eval (AndExpresion spec other) specs = Eval spec specs && Eval other specs
    Eval (OrExpression spec other) specs = Eval spec specs || Eval other specs
    Eval (NotExpression spec)      specs = Not (Eval spec specs)

type Satisfied :: Expression a -> [a] -> Constraint
type family Satisfied expression specifications where
    Satisfied expression specifications = Eval expression specifications ~ 'True