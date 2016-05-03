min' :: (Int, Int) -> Int
min' (0, y) = 0
min' (x, y) = x + y - max' (x, y)

max' :: (Int, Int) -> Int
max' (0, y) = y
max' (x, y) = (max 0 ((x - 1) - y)) + y

f :: (Integer, Integer) -> Integer
f (0, x) = 2 * x
f (n, x) = f (n - 1, f (n - 1, x + 1))

f' :: (Integer, Integer) -> Integer
f' (0, x) = 2 * x
f' (n, 0) = f (n - 1, 0) + 2 ^ (2 ^ (n - 1)) * (f (n - 1, 0) + 2 ^ (2 ^ (n - 1)))
f' (n, x) = 2 ^ (2 ^ n) + f(n, x - 1)
