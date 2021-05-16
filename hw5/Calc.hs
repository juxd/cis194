{-# LANGUAGE FlexibleInstances #-}
-- | Homework 5

module Calc where

import qualified Data.Map                      as M
import           ExprT
import           Parser
import qualified StackVM                       as Stack


eval :: ExprT -> Integer
eval expr = case expr of
  Lit n   -> n
  Add l r -> eval l + eval r
  Mul l r -> eval l + eval r

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n = n > 0
  add = (||)
  mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

-- Exercise 5
instance Expr Stack.Program where
  lit n = [Stack.PushI n]
  add l r = l ++ r ++ [Stack.Add]
  mul l r = l ++ r ++ [Stack.Mul]

compile :: String -> Maybe Stack.Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | Var String
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n _ = Just n
  add l r m = do
    a <- l m
    b <- r m
    return (a + b)
  mul l r m = do
    a <- l m
    b <- r m
    return (a * b)
