-- Exercise 1.2. Define a monotone operator whose minimal fixed points are:
-- a) The perfect squares
-- b) All powers of a
-- c) The Fibonacci numbers
-- d) The prime numbers

import Test.HUnit

import qualified Data.Set as Set
import qualified Data.List as List

-- a)
isPerfectSquare :: Int -> Bool
isPerfectSquare x = any (\y -> y * y == x) [1..x - 1]

nearestPerfectSquare :: Int -> Int
nearestPerfectSquare x
  | isPerfectSquare x = x
  | otherwise = nearestPerfectSquare $ succ x

perfectSquares :: Set.Set Int -> Set.Set Int
perfectSquares set = Set.singleton 1 `Set.union` Set.map (nearestPerfectSquare . succ) set

-- b)
powers :: Int -> Set.Set Int -> Set.Set Int
powers a set = Set.singleton 1 `Set.union` Set.map (*a) set

-- c)
fib :: Set.Set Int -> Set.Set Int
fib set = Set.fromList [0, 1, 2] `Set.union` Set.map next (Set.filter (/= 0) set)
    where next x = x + Set.findMax (Set.filter (< x) set)

-- d)
setProd :: Set.Set Int -> Int
setProd set = Set.foldr (*) 1 set

isPrime :: Int -> Bool
isPrime x = and [x /= y * z | y <- [2..x], z <- [2..x]]

primesLessThan :: Int -> [Int]
primesLessThan x = filter isPrime [2..x]

primes :: Set.Set Int -> Set.Set Int
primes set = Set.singleton 2 `Set.union` Set.fromList (primesLessThan $ setProd set + 1)

-- Test helpers
-- (Hopefully) get the smallest n elements of the fixed point of the operator
fixedPointHead :: (Set.Set Int -> Set.Set Int) -> Int -> [Int]
fixedPointHead operator n =
    take n . List.sort . Set.toList . head $ dropWhile ((<n) . Set.size) $ iterate operator Set.empty

main = runTestTT $ test [
    "perfectSquares produces consecutive perfect squares" ~:
        [1, 4, 9, 16, 25, 36, 49] @=? fixedPointHead perfectSquares 7,

    "powers produces consecutive powers of a" ~:
        [1, 2, 4, 8, 16, 32, 64] @=? fixedPointHead (powers 2) 7,

    "fib produces consecutive Fibonacci numbers" ~:
        [0, 1, 2, 3, 5, 8, 13] @=? fixedPointHead fib 7,

    "primes produces consecutive primes" ~:
        [2, 3, 5, 7, 11, 13, 17] @=? fixedPointHead primes 7
  ]
