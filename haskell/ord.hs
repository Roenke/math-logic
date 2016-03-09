-- Реализуйте ординалы до ε_0, используя нормальную форму Кантора.
data Ordinal = Undefined deriving Eq

-- Функция возвращает канонический представитель класса эквивалентности.
-- Она вам понадобится для реализации некоторых функций.
reduce :: Ordinal -> Ordinal
reduce = undefined

instance Ord Ordinal where
    compare = undefined

instance Show Ordinal where
    show = undefined

instance Num Ordinal where
    (+) = undefined
    (*) = undefined
    fromInteger = undefined

-- функция x |-> ω^x
omegaExp :: Ordinal -> Ordinal
omegaExp = undefined

-- Тестики

omegaN :: Integer -> Ordinal
omegaN 0 = 1
omegaN n = omegaExp $ omegaN (n - 1)

omega :: Ordinal
omega = omegaN 1

main = do
    -- Примеры того как дожен работать show.
    -- В каждом случае указано, что выводит моя версия.
    -- У вас может отличаться по модулю того, что появляются какие-нибудь лишние константы или скобки, например, ω^(1) + 0.
    print omega -- ω
    print (omegaN 2) -- ω^(ω)
    print $ omegaExp (omegaN 2 + omega) + omegaExp (omegaN 3 + omega + omega) -- ω^(ω^(ω^(ω)) + ω + ω)
    
    -- Тесты на сложение.
    -- Везде должно быть True.
    print
        [ omegaN 3 + omegaN 4 == omegaN 4
        , omegaN 3 + omegaN 2 /= omegaN 3
        , omegaN 3 + omegaN 2 /= omegaN 2
        , omegaExp (omegaN 3 + omegaN 2) + omega + omega == omegaExp (omegaN 3 + omega) + omega + omegaExp (omegaN 3 + omegaN 2) + omega + omega
        ]
    
    -- Тесты на умножение.
    -- Везде должно быть True.
    print
        [ 0 * omegaN 2 == 0
        , omegaN 3 * 0 == 0
        , omegaN 4 * 3 == omegaN 4 + omegaN 4 + omegaN 4
        , 4 * omegaN 3 == omegaN 3
        , omega * omega * omega * omega == omegaExp 4
        , (omegaN 4 + omegaN 3) * omegaExp (omegaN 3 + omega) == omegaExp (omegaN 3 * 2 + omega)
        ]
    
    -- Тесты на сравнение.
    -- Везде должно быть True.
    print
        [ omegaN 3 < omegaN 4
        , omegaN 3 > omegaN 2
        , omegaExp (omegaN 2 + omega) > omegaExp omega
        , omegaExp (omegaN 3 + omegaN 2) + omega + omega > omegaExp (omegaN 3 + omegaN 2) + omega
        , omegaN 5 > omegaExp (omegaN 3 + omegaN 3 + omegaN 2 + omega)
        , omegaN 4 < omegaExp (omegaN 3 + omegaN 3 + omegaN 2 + omega)
        , omegaN 4 + omega > omegaN 4 + 7
        , omegaN 4 * omega > omegaN 4 * 7
        ]
