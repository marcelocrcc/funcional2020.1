-- "Ordenação"
-- https://github.com/senapk/funcional_arcade#ordena%C3%A7%C3%A3o


--    @046 compac
{-

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

--    @003 vetFib
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

--    @034 ordenada
ordenada (x:xs)
    | xs == []                    = True
    | and $ map (\y -> x <= y) xs = True && ordenada xs
    | otherwise                   = False

--    @033 inserir - inserir ordenado
inserir x u 
    | u == []    = x:u 
    | x < head u = x:u 
    | otherwise  = head u:inserir x (tail u)

--    @045 bubble
--    @035 qsort
--    @036 merge - recursão
--    @057 buscabin - busca binária

