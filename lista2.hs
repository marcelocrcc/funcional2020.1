-- "Lista 2"
-- https://github.com/senapk/funcional_arcade#lista-2

-- 01. paridade

paridade u = odd $ length $ filter (==True) u

-- 02. rev
-- acho essa questão meio confusa, tenho que imprimir "0002" se o número for "2000"?
-- não consegui fazer
-- lembro que ela é imensa em C sem usar vetores

-- essa é a resposta do Igor, ela imprime "2" se o número for "2000"
--rev 0 = 0
--rev x = (x `mod` 10) * round (10 ** ((numCasas x) - 1)) + (rev (x `div` 10))
--    where numCasas x = (floor (logBase 10 x)) + 1


-- 03. deletee
--deletee :: a -> [a] -> [a]
deletee x u 
    | u == []     = []
    | x == head u = tail u
    | otherwise   = head u:deletee x (tail u)

-- 04. swap
-- tentei fazer esse recursivamente, acho que não dá sem uma função auxiliar?
-- também não entendi como faria com função auxiliar

--swap u p q
--    | p > (length u - 1) || q > (length u - 1) = u
--    | otherwise -- ...

-- não fiz as outras

