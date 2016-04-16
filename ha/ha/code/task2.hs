fun :: (a, b) -> (a -> b -> c) -> c
fun p f = f (fst p) (snd p)

fst' :: (a, b) -> a
fst' p = fun p (\a -> \b -> a)

snd' :: (a, b) -> b
snd' p = fun p (\a -> \b -> b)
