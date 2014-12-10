data Tree   = Leaf Integer
            | Node Tree Integer Tree
            deriving (Show, Eq)
            
occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
    = case compare m n of
        LT -> occurs m l
        EQ -> True
        GT -> occurs m r

{-- 

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
    = case compare m n of
        LT -> occurs m l
        EQ -> True
        GT -> occurs m r

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
    = case compare m n of
        LT -> occurs m r
        EQ -> True
        GT -> occurs m l

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = compare m n
occurs m (Node l n r)
    = case compare m n of
        LT -> occurs m l
        EQ -> True
        GT -> occurs m r

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
    | m == n = True
    | m < n = occurs m l
    | otherwise = occurs m r
--}

