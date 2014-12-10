import Data.List
import Data.Char
import Unsafe.Coerce
import Test.HUnit

data Nat = Zero
          | Succ Nat
          deriving Show

-- natToInteger    Zero        =   0
-- natToInteger    (Succ n)    =   natToInteger n + 1

-- natToInteger = head . m
--    where   m Zero      = [0]
--            m (Succ n)  = [sum [x | x <- (1 : m n)]]

natToInteger :: Nat -> Integer
natToInteger    = \n -> genericLength [c | c <- show n, c == 'S']

tests = TestList
        [ "natToInteger 1" ~: natToInteger Zero ~?= 0
        , "natToInteger 2" ~: natToInteger (Succ Zero) ~?= 1
        , "natToInteger 3" ~: natToInteger (Succ (Succ Zero)) ~?= 2
        ]