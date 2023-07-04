module Output where

import Data.Bits

func :: Int
func = 0

func1 :: Int
func1 = a
  where
    a :: Int
    a = 2
    arr :: [Int]
    arr = [1, 2, 3]

func2 :: Bool
func2 = b
  where
    a :: Int
    a = 2
    b :: Bool
    b = True

func3 :: Int -> Bool
func3 c = b
  where
    a :: Int
    a = a .|. 4
    b :: Bool
    b = 4 >= c
    z :: Bool
    z = True && False

function :: Int -> Int -> Bool
function a b = c
  where
    c :: Bool
    c = a > b
    d :: Bool
    d = 2 >= 3

ifFunction :: Int -> Int -> String
ifFunction a b = result
  where
    result = if a > b then "mayor" else "menor"

apply :: Int
apply = 3

functionCall :: Int
functionCall = res2
  where
    e :: Int
    e = 2
    f = (function)
    res :: Int
    res = e .&. 3
    nextRes = (apply)
    res2 :: Int
    res2 = 2
