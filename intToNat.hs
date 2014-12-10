{-# LANGUAGE NPlusKPatterns #-}
import Data.List
import Data.Char
import Unsafe.Coerce
import Test.HUnit

data Nat = Zero
          | Succ Nat
          deriving Show          
        
-- integerToNat 0      = Zero
-- integerToNat (n+1)  = Succ (integerToNat n)

-- integerToNat (n+1)  = let m = integerToNat n in Succ m
-- integerToNat 0      = Zero

-- integerToNat 0     = Zero
-- integerToNat n     = Succ (integerToNat (n-1))

integerToNat :: Integer -> Nat
integerToNat = \n -> genericLength [c | c <- show n, isDigit c]