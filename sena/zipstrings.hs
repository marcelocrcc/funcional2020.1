-- "Zip"
-- https://github.com/senapk/funcional_arcade#zip
-- "Strings"
-- https://github.com/senapk/funcional_arcade#strings
import Data.Char


--    @064 produtoEscalar - utilizando a função zip
produtoEscalar xs ys = foldr (\x y-> (fst x) * (snd x) + y) 0 (zip xs ys)

--    @065 indices - busca posições do elemento
indices v xs = foldr (\x y -> if snd x == v then [fst x] ++ y else [] ++ y) [] (zip [0..] xs)
 
--    @039 upper
upper s = map (\x -> if isLowerChar x then up x else x) s
    where isLowerChar a = or $ map (==a) ['a'..'z']
          up x   = snd $ ((filter (\y -> x == fst y) mapper) !! 0) 
          mapper = zip ['a'..'z'] ['A'..'Z']

--    @040 titulo
titulo' s
    | s == []       = []
    | head s == ' ' = head s:toUpper (head (tail s)):titulo' (tail $ tail s)
    | otherwise     = toLower (head s):titulo' (tail s)

titulo s = toUpper(head s):tail (titulo' s)


--    @041 selec - apenas as chaves selecionadas
selec u p = map (u !!) p

--    @042 isPalind - verificar palíndromo
-- descobri depois que basta espelhar o vetor...
-- aqui ele compara a primeira metade com o reverso da segunda
isPalind s
    | odd $ length s = take ((length s + 1) `div` 2) s == reverse (drop ((length s) `div` 2) s)
    | otherwise      = take ((length s) `div` 2) s     == reverse (drop ((length s) `div` 2) s)

