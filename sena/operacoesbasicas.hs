-- "Operações Básicas"
-- https://github.com/senapk/funcional_arcade#opera%C3%A7%C3%B5es-b%C3%A1sicas

--    @007 somaImpares
--    o foldr começa a somar com 0, aumentando apenas com os números ímpares
somaImpares xs = foldr (+) 0 (filter odd xs) 

--    @001 max3
max3 :: Ord a => a -> a -> a -> a
max3 x y z 
    | x > y && x > z = x 
    | y >= z         = y
    | otherwise      = z 

--    @012 fatorial
fatorial 0 = 1 
fatorial n = n * fatorial (n-1)

--    @014 elemento
elemento n xs
    | n >= 0    = xs !! n
    | otherwise = xs !! (length xs + n)

--    @015 pertence
pertence x u = not $ null $ filter (==x) u

--    @016 total
total u = sum $ map (const 1) u

--    @017 maior
maior u
    | and (map (<= (head u)) u) == True = head u
    | otherwise                         = maior ((tail u) ++ [head u])

--    @023 corpo
corpo u = init u

--    @028 divide
divide u n = (take n u, drop n u)

--    @030 uniao
uniao' u
    | u == []   = []
    | otherwise = head u:(uniao' (filter (/= head u) u))

uniao a b = uniao' (a ++ b)

--    @031 intersec - intercessão entre listas
intersec' u
    | u == []                         = []
    | or $ map (== (head u)) (tail u) = head u:intersec' (tail u)
    | otherwise                       = intersec' (tail u)

intersec a b = intersec' (a ++ b)

--    @047 splitints
splitints u = (filter odd u, filter even u)

--    @009 sublist
--    esse é antigo
--    desculpa, professor...
sublist a b xs
    | a >= 0 && b >= 0 = reverse $ drop (length xs - b) (reverse $ drop a                   xs)
    | a >= 0 && b <  0 = reverse $ drop (abs b)         (reverse $ drop a                   xs)
    | a <  0 && b >= 0 = reverse $ drop (length xs - b) (reverse $ drop (length xs - abs a) xs) 
    | a <  0 && b <  0 = reverse $ drop (abs b)         (reverse $ drop (length xs - abs a) xs) 

--    @051 paridade
paridade u = odd $ length $ filter (==True) u

--    @054 swap - trocando dois elementos de uma lista
-- tentei fazer esse recursivamente, acho que não dá sem uma função auxiliar?
-- também não entendi como faria com função auxiliar

--swap u p q
--    | p > (length u - 1) || q > (length u - 1) = u
--    | otherwise -- ...

--    @063 euler1 - soma dos múltiplos de 3 e 5
euler1 limit = sum [x | x <- [3..limit-1], (mod x 3 == 0 || mod x 5 == 0)]
