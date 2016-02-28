-- Сконструируйте биекцию между { 0 .. n-1 } + { 0 .. k-1 } и { 0 .. n+k-1 }
sum_bij
    :: Int -- мощность первого множества.
    -> Int -- мощность второго множества.
    -> (Either Int Int -> Int, Int -> Either Int Int) -- требуемая биекция.
                                                      -- предполагается, что в функции не передаются числа, выходящие за пределы множеств, которые указаны в условии.
sum_bij n k = (f, g) where
    f (Left a) = a
    f (Right b) = n + b
    g p | p < n = Left p
        | otherwise = Right (p - n)

-- Сконструируйте биекцию между { 0 .. n-1 } * { 0 .. k-1 } и { 0 .. n*k-1 }
mul_bij
    :: Int -- мощность первого множества.
    -> Int -- мощность второго множества.
    -> ((Int,Int) -> Int, Int -> (Int,Int)) -- требуемая биекция.
                                            -- предполагается, что в функции не передаются числа, выходящие за пределы множеств, которые указаны в условии.
mul_bij n k = (f, g) where
    f (a, b) = a * k + b
    g p      = (p `div` k, p `mod` k)

-- Сконструируйте биекцию между { 0 .. n-1 } -> { 0 .. k-1 } и { 0 .. k^n-1 }
exp_bij
    :: Int -- мощность первого множества.
    -> Int -- мощность второго множества.
    -> ((Int -> Int) -> Int, Int -> (Int -> Int)) -- требуемая биекция.
                                                  -- предполагается, что в функции не передаются числа, выходящие за пределы множеств, которые указаны в условии.
toBase_g 0 value base     = [] 
toBase_g count value base = (value `mod` base) : (toBase_g (count - 1) (value `div` base) base)

exp_bij n k = (f, g) where 
    f fun = sum (zipWith (*) values degrees) where
        values  = map fun [0..(n - 1)]
        degrees = zipWith (^) (replicate n k) [0..(n - 1)]
    g x   = (\y -> values !! y) where 
        values = toBase n x k where
            toBase 0 value base     = []
            toBase count value base = (value `mod` base) : (toBase (count - 1) (value `div` base) base)

-- Проверяем функции
test :: (a -> Int, Int -> a) -> Int -> Int
test (f,g) n = f (g n)

main = do
    print $ map (test (sum_bij 8 12)) [0..19]
    print $ map (test (mul_bij 12 8)) [0..95]
    print $ map (test (exp_bij 5 4)) [0..1023]
