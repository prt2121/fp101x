import Data.List
import Data.Char
import Unsafe.Coerce
import Test.HUnit

data Nat = Zero
          | Succ Nat
          deriving Show

-- add :: Nat -> Nat -> Nat
-- add Zero        n = n
-- add (Succ m)    n = Succ (add n m)

add :: Nat -> Nat -> Nat
add n Zero          = n
add n (Succ m)      = Succ (add m n)

mult m Zero         = Zero
mult m (Succ n)     = add m (mult m n)