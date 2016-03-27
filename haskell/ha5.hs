two_a :: p -> p
two_a = id

two_b :: p -> (p -> q) -> q
two_b x f = f x

two_c :: (p, q) -> Either p q
two_c p = case Left(fst p) of 
    Right(p') -> Right(snd p)
    Left(p') -> Left(p')

two_d :: (Either p q, r) -> Either (p, r) (q, r)
two_d p = case (fst p) of 
    Left a -> Left (a, snd p)
    Right b -> Right (b, snd p)

three_a :: Either (p, r) (q, r) -> (Either p q, r)
three_a e = case e of 
    Left p  -> (Left (fst p), snd p)
    Right p -> (Right (fst p), snd p)

three_b :: Either (Either p q) r -> Either p (Either q r)
three_b e = case e of
    Left e' -> case e' of 
        Left p -> Left p
        Right q -> Right (Left q)
    Right r -> Right (Right r)

three_c :: ((((p -> q) -> p) -> p) -> q) -> q
three_c x = x (\y -> y (\z -> (x (\p ->z))))
