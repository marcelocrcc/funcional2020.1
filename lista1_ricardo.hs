-- "Lista 1 - Ricardo"
-- https://github.com/senapk/funcional_arcade#lista-1---ricardo

-- é usado na função "titulo"
import Data.Char(toUpper)
import Data.Char(toLower)

menorDeDois x y
    | x <= y    = x
    | otherwise = y

menorDeTres x y z
    | x <= y && x <= z = x
    | y <= z           = y
    | otherwise        = z

fatorial n
    | n == 0    = 1
    | otherwise = n * fatorial (n - 1)

fibonacci n
    | n == 1    = 0
    | n == 2    = 1
    | otherwise = fibonacci (n - 2) + fibonacci (n - 1)

elemento u n
    | n == 0    = head u
    | otherwise = elemento (tail u) (n - 1)

pertence u x
    | u == []     = False
    | head u == x = True
    | otherwise   = pertence (tail u) x

total u
    | u == []   = 0
    | otherwise = total (tail u) + 1

maior u
    | and (map (<= (head u)) u) == True = head u
    | otherwise                         = maior ((tail u) ++ [head u])


frequencia u x = length $ filter (==x) u

unico u x = length (filter (==x) u) == 1

maioresQue u x = filter (>x) u

concat a b = a ++ b

calda u = tail u

corpo u = init u

unique u
    | u == []   = []
    | otherwise = head u:(unique (filter (/= head u) u))


-- retorna uma lista u em ordem crescente
menores' u
    | u == []                           = []
    | and (map (>= (head u)) u) == True = (head u):(menores' (tail u))
    | otherwise                         = menores' ((tail u) ++ [head u])

-- retorna os n menores números de uma lista
menores n u = filter (<= ((menores' u) !! (n - 1))) u 

-- menores' [5, 4, 1, 9, 7, 2]
-- [1, 2, 4, 5, 7, 9]
--
-- menores [1, 2, 4, 5, 7, 9]
-- indice (n - 1) -> valor 4
-- filter (<= 4) [5, 4, 1, 9, 7, 2] -> [4, 1, 2]



alter n
    | n == 0       = []
    | otherwise    = alter (n - 1) ++ [n] ++ [negate n]

reverso u
    | u == []   = []
    | otherwise = last u:reverso (init u)

divide u n = (take n u, drop n u)

intercal a b
    | a == [] = b
    | b == [] = a
    | otherwise = head a:head b:intercal (tail a) (tail b)

uniao' u
    | u == []   = []
    | otherwise = head u:(uniao' (filter (/= head u) u))

uniao a b = uniao' (a ++ b)


intersec' u
    | u == []                         = []
    | or $ map (== (head u)) (tail u) = head u:intersec' (tail u)
    | otherwise                       = intersec' (tail u)

intersec a b = intersec' (a ++ b)

sequencia n m = take (m + n - 1) [m..]

inserir x u
    | u == []    = x:u
    | x < head u = x:u
    | otherwise  = head u:inserir x (tail u)

isSorted u
    | u == []                    = True
    | and $ map (head u <=) u    = isSorted (tail u)
    | otherwise                  = False

-- não fiz essa, pesquisei no google
-- pensei que talvez não funcionasse porque... o pivô é sempre na esquerda?
qsort []   = []
qsort (x:xs)    = qsort left ++ [x] ++ qsort right
    where 
        left = filter (<= x) xs
        right = filter (> x) xs

rotEsq n s
    | n == 0    = s
    | otherwise = rotEsq (n - 1) (tail s ++ [head s])

rotDir n s
    | n == 0    = s
    | otherwise = rotDir (n - 1) (last s:init s)

upper s = map toUpper s

-- se for o primeiro é uppercase
-- se for um estpaço o próximo é uppercase
titulo' s
    | s == []       = []
    | head s == ' ' = head s:toUpper (head (tail s)):titulo' (tail $ tail s)
    | otherwise     = toLower (head s):titulo' (tail s)

titulo s = toUpper(head s):tail (titulo' s)

selec u p
    | p == []   = []
    | otherwise = u !! (head p):selec u (tail p)

-- descobri depois que basta espelhar o vetor...
-- aqui ele compara a primeira metade com o reverso da segunda
isPalind s
    | odd $ length s = take ((length s + 1) `div` 2) s == reverse (drop ((length s) `div` 2) s)
    | otherwise      = take ((length s) `div` 2) s     == reverse (drop ((length s) `div` 2) s) 

-- tá difícil de ler, mas é assim:
-- o round é usado pra converter o Float para Num
-- mapeia todos os números menores que ele até 2 com (mod n)
--      mapeia o vetor resultante com (/= 0)
--          vẽ se todos os elementos do vetor resultante são True
primo :: Float -> Bool
primo 1 = False
primo n = and (map (/= 0) (map (mod (round n)) (take ((round ((sqrt n) - 1))) [2..])))

-- diminui o número até ele ser zero, somando cada dígito subtraído
sdig :: (Integral a) => a -> a
sdig n
    | n == 0        = 0
    | mod n 10 == 0 = sdig (floor ((fromIntegral n)/10))
    | otherwise     = 1 + (sdig (n - 1))


-- não consegui imaginar como fazer...
--bubblesort (x:y:u)
--    | u == [] = (x:y:u)
--    | x <= y  = x : bubblesort (y:u)
--    | x >  y  = y : bubblesort (x:u)

{-
-- o exemplo na lista do Ricardo tá errado
-- essa função não funciona (mas na minha opinião deveria)

-- recebe lista, retorna lista de tuplas
compac'' u
    | u == []                             = []
    | (length $ filter (==head u) u) == 1 = (1, head u)                               : compac'' (filter (/= head u) (tail u))
    | otherwise                           = ((length $ filter (== head u) u), head u) : compac'' (filter (/= head u) (tail u))

-- recebe lista de tuplas, retorna lista de listas
compac' u
    | (compac'' u) == []      = [[]]
    | fst (head (compac'' u)) == 1 = [snd (head $ compac'' u)]                          : compac' (tail $ compac'' u)
    | otherwise                    = [fst (head $ compac'' u), snd (head $ compac'' u)] : compac' (tail $ compac'' u)

compac u = (compac' (compac'' u))
-}

splitints u = (filter odd u, filter even u)

perfeito n = or [(x*x) == n | x <- [1..n]]

-- não fiz as outras
