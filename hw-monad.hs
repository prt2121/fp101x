putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putChar '\n' 

getLine' = get []
get :: String -> IO String
get xs = do 
  x <- getChar
  case x of
    '\n' -> return xs
    _ -> get (xs ++ [x])

interact' :: (String -> String) -> IO ()
interact' f = do
  x <- getLine'
  putStrLn' (f x)

sequence_' :: Monad m => [m a] -> m ()
sequence_' ms = foldr (>>) (return ()) ms

sequence_'' [] = return ()
sequence_'' (m : ms) = (foldl (>>) m ms) >> return ()

sequence_''' [] = return ()
sequence_''' (m : ms) = m >> sequence_''' ms

sequence' [] = return []
sequence' (m : ms)
  = m >>=
    \a ->
     do as <- sequence' ms
        return (a : as)

sequence1' :: Monad m => [m a] -> m [a]
sequence1' ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m[a]
    func m acc 
      = do x <- m
           xs <- acc
           return (x : xs)

sequence2' [] = return []
sequence2' (m : ms) 
  = do a <- m
       as <- sequence2' ms
       return (a : as) 

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence' (map f as)

mapM'' f [] = return []
mapM'' f (a : as)
	= f a >>= \ b -> mapM'' f as >>= \ bs -> return (b : bs)

mapM''' f [] = return []
mapM''' f (a : as) = do
  b <- f a
  bs <- mapM''' f as
  return (b : bs)

filterM' _ [] = return []
filterM' p (x : xs)
  = do flag <- p x
       ys <- filterM' p xs
       if flag then return (x : ys) else return ys
