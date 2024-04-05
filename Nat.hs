data Nat = Zero
         | Suc Nat
         deriving Show

one = Suc Zero
two = Suc (Suc Zero)
three = Suc (Suc (Suc Zero))
four = Suc (Suc (Suc (Suc Zero)))

nat2integer :: Nat -> Int
nat2integer Zero = 0
nat2integer (Suc n) = 1 + nat2integer n

integer2nat :: Int -> Nat
integer2nat 0 = Zero
integer2nat n = Suc(integer2nat(n - 1))

natAdd :: Nat -> Nat -> Nat
natAdd n Zero = n
natAdd n (Suc m) = natAdd (Suc n) m

natSub :: Nat -> Nat -> Nat
natSub Zero _ = Zero
natSub n Zero = n
natSub (Suc n) (Suc m) = natSub n m

natMul :: Nat -> Nat -> Nat
natMul Zero _ = Zero
natMul _ Zero = Zero
natMul n (Suc m) = natAdd n (natMul n m)