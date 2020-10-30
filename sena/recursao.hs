--- "Recursão"
--  https://github.com/senapk/funcional_arcade#recurs%C3%A3o

-- @013 fibonacci
fibonacci n
    | n == 1    = 0 
    | n == 2    = 1 
    | otherwise = fibonacci (n - 2) + fibonacci (n - 1)

-- @018 frequencia
frequencia [] _ = 0
frequencia x u = if head u == x then 1 + frequencia (tail u) x else 0 + frequencia (tail u) x

-- @019 unico
-- unico x u = length (filter (==x) u) == 1
unico x u  = 1 == total x u
    where
        total _ [] = 0 
        total x u  = (if head u == x then 1 + total x (tail u) else 0 + total x (tail u))

-- @020 maioresQue
-- maioresQue u x = filter (>x) u

-- @021 concatena
-- concatena = (++)
concatena xs ys = foldr (:) (foldr (:) [] ys) xs

-- @025 menores
-- retorna uma lista u em ordem crescente

{-

-- não funciona
-- nem faz sentido, na verdade

menores' u
    | u == []                           = []
    | and (map (>= (head u)) u) == True = (head u):(menores' (tail u)) 
    | otherwise                         = menores' ((tail u) ++ [head u]) 

-- retorna os n menores números de uma lista
menores n u = filter (<= ((menores' u) !! (n - 1))) u

menores_cres = take n (menores' u)
-- menores' [5, 4, 1, 9, 7, 2]
-- [1, 2, 4, 5, 7, 9]
--
-- menores [1, 2, 4, 5, 7, 9]
-- indice (n - 1) -> valor 4
-- filter (<= 4) [5, 4, 1, 9, 7, 2] -> [4, 1, 2]

-}

-- @026 alter
alter n
    | n == 0       = []
    | otherwise    = alter (n - 1) ++ [n] ++ [negate n]

-- @027 reverso
reverso u
    | u == []   = []
    | otherwise = last u:reverso (init u)

-- @029 intercal - intercalar duas listas
intercal a b
    | a == [] = b
    | b == [] = a
    | otherwise = head a:head b:intercal (tail a) (tail b)

-- @032 sequencia
sequencia n m = take (m + n - 1) [m..]

-- @037 rotEsq
rotEsq n s
    | n == 0    = s
    | otherwise = rotEsq (n - 1) (tail s ++ [head s])

-- @038 rotDir
rotDir n s
    | n == 0    = s
    | otherwise = rotDir (n - 1) (last s:init s)


-- @048 quadperf
quadperf x = quadperf' x 1
    where 
        quadperf' x y
            | x == y*y       = True
            | x == y         = False
            | otherwise      = quadperf' x (y + 1)

-- @053 deletee - remover primeira ocorrência
deletee x u 
    | u == []     = []
    | x == head u = tail u
    | otherwise   = head u:deletee x (tail u)

-- @059 listacc - lista acumulativa
listacc xs = listacc' 1 xs
    where 
        listacc' x xs
            | x == length xs + 1 = []
            | xs == []           = []
            | otherwise = (sum $ take x xs) : listacc' (x+1) xs


-- @061 line - linhas de um triângulo aritmético
line n = line' n n
    where
        line' n 0 = []
        line' n x = (sum [1..n-1] + 1) + (n - x) : line' n (x - 1)

-- @062 triangle - triângulo aritmético
triangle n = triangle' n n
    where
        triangle' n 0 = []
        triangle' n x = [line (n - x + 1)] ++ triangle' n (x - 1)
