--- FILE: src/Types.hs
module Types
  ( Expr(..)
  , Number(..)
  ) where

-- Tipos algebraicos de datos
data Number = I Integer | D Double
  deriving (Eq, Show)

-- Expresiones simples para la calculadora (ejemplo de ADT recursivo)
data Expr
  = Lit Number
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Fact Expr
  deriving (Eq, Show)

--- FILE: src/Functions.hs
module Functions
  ( factorial
  , pow
  , evalExpr
  , (|>)
  , compose
  , curry2
  , partialPow
  ) where

import Types

-- Funciones puras y recursivas
factorial :: Integer -> Integer
factorial n
  | n <= 1    = 1
  | otherwise = n * factorial (n - 1)

pow :: (Num a, Integral b) => a -> b -> a
pow _ 0 = 1
pow x n
  | n < 0     = error "pow: negative exponent"
  | otherwise = x * pow x (n - 1)

-- Evaluador simple para el AST
evalExpr :: Expr -> Either String Double
evalExpr (Lit (I i)) = Right (fromIntegral i)
evalExpr (Lit (D d)) = Right d
evalExpr (Add a b) = binaryOp (+) a b
evalExpr (Sub a b) = binaryOp (-) a b
evalExpr (Mul a b) = binaryOp (*) a b
evalExpr (Div a b) = do
  vb <- evalExpr b
  if vb == 0 then Left "Division by zero" else do
    va <- evalExpr a
    Right (va / vb)
evalExpr (Pow a b) = do
  va <- evalExpr a
  vb <- evalExpr b
  Right (va ** vb)
evalExpr (Fact e) = do
  v <- evalExpr e
  if v < 0 || (v /= fromInteger (round v)) then Left "factorial: non-natural"
  else Right . fromIntegral . factorial . round $ v

binaryOp :: (Double -> Double -> Double) -> Expr -> Expr -> Either String Double
binaryOp f a b = do
  va <- evalExpr a
  vb <- evalExpr b
  Right (f va vb)

-- Operadores funcionales: composición simple y piping
(|>) :: a -> (a -> b) -> b
x |> f = f x

compose :: (b -> c) -> (a -> b) -> a -> c
compose = (.)

-- Currying y aplicación parcial (demostración)
curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f a b = f (a, b)

partialPow :: (Num a, Integral b) => a -> (b -> a)
partialPow x = pow x

--- FILE: src/Sequences.hs
module Sequences
  ( naturals
  , fromTo
  , takeN
  , mapSeq
  , filterSeq
  , foldSeq
  ) where

-- Generadores de secuencias (lazy)
naturals :: [Integer]
naturals = [0..]

fromTo :: Integer -> Integer -> [Integer]
fromTo a b = [a..b]

takeN :: Int -> [a] -> [a]
takeN = take

-- Operaciones sobre secuencias (trabajan lazy)
mapSeq :: (a -> b) -> [a] -> [b]
mapSeq = map

filterSeq :: (a -> Bool) -> [a] -> [a]
filterSeq = filter

foldSeq :: (b -> a -> b) -> b -> [a] -> b
foldSeq = foldl

--- FILE: src/ListUtils.hs
module ListUtils
  ( mapF
  , filterF
  , reduceF
  , foldrF
  , zipWithF
  , sliding
  ) where

-- Implementaciones funcionales explícitas
mapF :: (a -> b) -> [a] -> [b]
mapF _ []     = []
mapF f (x:xs) = f x : mapF f xs

filterF :: (a -> Bool) -> [a] -> [a]
filterF _ [] = []
filterF p (x:xs)
  | p x       = x : filterF p xs
  | otherwise = filterF p xs

reduceF :: (a -> a -> a) -> a -> [a] -> a
reduceF _ acc [] = acc
reduceF f acc (x:xs) = reduceF f (f acc x) xs

foldrF :: (a -> b -> b) -> b -> [a] -> b
foldrF _ z []     = z
foldrF f z (x:xs) = f x (foldrF f z xs)

zipWithF :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithF _ [] _ = []
zipWithF _ _ [] = []
zipWithF f (x:xs) (y:ys) = f x y : zipWithF f xs ys

sliding :: Int -> [a] -> [[a]]
sliding n xs
  | n <= 0    = []
  | length xs < n = []
  | otherwise = take n xs : sliding n (tail xs)

--- FILE: src/Tree.hs
module Tree
  ( Tree(..)
  , insert
  , search
  , inorder
  , preorder
  , postorder
  ) where

-- Árbol binario funcional
data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node v l r)
  | x == v = Node v l r
  | x < v  = Node v (insert x l) r
  | x > v  = Node v l (insert x r)

search :: (Ord a) => a -> Tree a -> Bool
search _ Empty = False
search x (Node v l r)
  | x == v = True
  | x < v  = search x l
  | otherwise = search x r

-- Traversals
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node v l r) = inorder l ++ [v] ++ inorder r

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node v l r) = [v] ++ preorder l ++ preorder r

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node v l r) = postorder l ++ postorder r ++ [v]

--- FILE: src/LazyEval.hs
module LazyEval
  ( naturalsFrom
  , fibs
  , takeStream
  , strictSum
  , lazySum
  ) where

-- Streams infinitos usando listas perezosas
naturalsFrom :: Integer -> [Integer]
naturalsFrom n = n : naturalsFrom (n + 1)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

takeStream :: Int -> [a] -> [a]
takeStream = take

-- Comparación entre evaluación estricta y perezosa
-- strictSum fuerza la evaluación completo (demostración) usando foldl' sería mejor
strictSum :: Num a => [a] -> a
strictSum = foldl (+) 0

lazySum :: Num a => [a] -> a
lazySum = foldr (+) 0

--- FILE: app/Main.hs
module Main where

import Types
import Functions
import Sequences
import ListUtils
import Tree
import LazyEval

import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- Un mini-runner con casos de prueba simples e impresión de métricas
timeIt :: IO a -> IO (a, Double)
timeIt action = do
  start <- getCPUTime
  r <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  return (r, diff)

main :: IO ()
main = do
  putStrLn "== Sistema funcional: demostración rápida =="
  -- Factorial
  let n = 10
  putStrLn $ "factorial (recursivo) de " ++ show n ++ " = " ++ show (factorial n)

  -- Evaluación de expresiones
  let e = Add (Lit (I 5)) (Mul (Lit (I 3)) (Lit (I 2)))
  putStrLn $ "Eval Expr: " ++ show e ++ " => " ++ show (evalExpr e)

  -- Streams
  putStrLn $ "Primeros 10 naturales: " ++ show (takeStream 10 naturals)
  putStrLn $ "Primeros 10 fibs: " ++ show (takeStream 10 fibs)

  -- Árbol
  let vals = [7,3,9,1,5,8,10]
  let t = foldr insert Empty vals
  putStrLn $ "Inorder tree: " ++ show (inorder t)
  putStrLn $ "Search 5: " ++ show (search 5 t)

  -- Performance demo: tiempo de map sobre lista grande
  let big = take 1000000 naturals
  (_, tmap) <- timeIt $ return $ map (+1) big
  printf "Tiempo map 1e6: %.6f s\n" tmap

  putStrLn "== Fin demostración =="

--- FILE: test/TestSuite.hs
module TestSuite where

import Test.HUnit
import Functions
import Tree

case_factorial = TestCase (assertEqual "factorial 5" 120 (factorial 5))
case_insert_search = TestCase (
  let t = foldr insert Empty [3,1,4,2] in
  assertBool "search 2" (search 2 t))

tests = TestList [TestLabel "fact" case_factorial, TestLabel "tree" case_insert_search]

runTests = runTestTT tests