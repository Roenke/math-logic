import Language.Haskell.Djinn

task5 :: (Either (p -> q) (q -> p) -> p) -> p
task5 f = f (Right (\q -> f (Left (\p -> q))))

--task1 :: ((p -> q) -> p) -> p
--task1 f = f (\p -> p)

data Type = TVar String | Arr Type Type | Prod Type Type | Sum [Type]
    deriving Show

bot :: Type
bot = Sum []

neg :: Type -> Type
neg p = Arr p bot

-- В термах добавлена информация о типах, чтобы можно было легко вывести тип терма.
-- В лямбде Lam x p t, p -- это тип x.
-- В Case t p cs, p -- это тип всего выражения Case t p cs, а также тип каждого терма в списке cs.
data Term = Var String | Lam String Type Term | App Term Term | Pair Term Term | Fst Term | Snd Term | In Int Term | Case Term Type [(String,Term)] | Lem Type
    deriving Show

-- getType t возвращает Just (тип t), если терм корректен, иначе возвращает Nothing.
getType :: Term -> Maybe Type
getType = undefined

-- getTerm p возвращает Just (терм типа p), если такой терм существует, иначе возвращает Nothing.
getTerm :: Type -> Maybe Term
getTerm p = go [] [p]
  where
    -- go принимает списки формул [p_1, ... p_n] и [q_1, ... q_k] и вовзращает Just (терм, доказывающий секвенцию p_1, ... p_n |- q_1, ... q_k), если такой существует, иначе Nothing.
    go :: [Type] -> [Type] -> Maybe Term
    go = undefined

-- Если t имеет тип вида neg p, то getIntuitionisticTerm t должна возвращать t', такой что t' имеет такой же тип, что и t, но не использует Lem.
-- Если тип t не имеет вид neg p, то поведение функции может быть любым.
getIntuitionisticTerm :: Term -> Term
getIntuitionisticTerm = undefined

vp :: Type
vp = TVar "P"

vq :: Type
vq = TVar "Q"

main = do
    print $ getTerm $ Sum []
    print $ getTerm $ Arr vp vq
    print $ getTerm $ Sum [Arr vp vq, Arr vq vp]
    print $ getTerm $ Arr (Arr (Sum [Arr vp vq, Arr vq vp]) vp) vp
    print $ getTerm $ Arr (Arr vp vq) (Sum [neg vp, vq])
    print $ getTerm $ neg $ neg $ Arr (Arr vp vq) (Sum [neg vp, vq])
    print $ getTerm $ neg $ Prod (Prod (neg $ neg $ Arr vp vq) (neg $ neg vp)) (neg vq)
    
    print $ fmap getIntuitionisticTerm $ getTerm $ neg $ neg $ Arr (Arr vp vq) (Sum [neg vp, vq])
    print $ fmap getIntuitionisticTerm $ getTerm $ neg $ Prod (Prod (neg $ neg $ Arr vp vq) (neg $ neg vp)) (neg vq)
