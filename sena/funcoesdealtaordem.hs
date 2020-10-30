import Data.List
import Data.Char

--- concatenaFold
-- Escreva a definição da função concatenaFold :: [[a]] -> [a] que concatena uma lista de listas usando a função foldr::(a -> b -> b) -> b -> [a] -> b.
-- concatenaFold [[1,2],[3,4]] == [1,2,3,4]
-- concatenaFold [[1,2],[3,4],[6,7,8]] == [1,2,3,4,6,7,8]

concatenaFold :: [[a]] -> [a]
concatenaFold = foldr (++) []

--- inverteFold
-- Escreva a definição da função inverteFold :: [a] -> [a] tal que (inverteFold xs ) devolve a lista xs invertida usando a função foldr::(a -> b -> b) -> b -> [a] -> b. 
-- Não utilize a função reverse do módulo Prelude.
-- inverteFold [1,2,3,4] == [4,3,2,1]

inverteFold :: [a] -> [a]
inverteFold = foldr (\x xs -> xs ++ [x]) []

--- paridadeFold
-- Escreva uma função paridadeFold :: [Bool] -> Bool que calcule a paridade de uma lista de boleanos): se o número de valores True for par então a paridade é True, caso contrário é False.
-- paridadeFold [True,True,False,True]      == False
-- paridadeFold [True,True,False,True,True] == True

f bool a
    | bool == True  = 1 + a
    | bool == False = 0 + a

paridadeFold :: [Bool] -> Bool
paridadeFold = even . foldr f 0

--- duplicarFold
-- A função duplicarFold :: String -> String repete duas vezes cada vogal (letras 'a', 'e', 'i', 'o', 'u' minúsculas ou maiúsculas) numa cadeia de carateres; os outros carateres devem ficar inalterados
-- duplicarFold "Ola, mundo!" == "OOlaa, muundoo!"

vogais = "aeiouAEIOU"

duplicar :: Char -> [Char] -> [Char]
duplicar a xs
    | elem a vogais = [a, a] ++ xs
    | otherwise     = [a]    ++ xs

duplicarFold :: Foldable t => t Char -> [Char]
duplicarFold = foldr duplicar []

--- filtraAplicaFold
-- Defina a função filtraAplicaFold :: (a->b) -> (a->Bool)->[a]->[b] tal que (filtraAplicaFold f p xs) é uma lista obtida aplicando a função f aos elementos de xs que satisfazem o predicado p usando a função foldr
-- filtraAplicaFold (4+) (<3) [1..7] == [5,6]

filtraAplicaFold :: (a->b) -> (a->Bool)->[a]->[b]
filtraAplicaFold f p xs = foldr f' [] xs
    where f' x ys = if p x then [f x] ++ ys else [] ++ ys

--- mapFold
-- Defina função mapFold :: (a->b) -> [a] -> [b] tal que (mapFold f xs) devolve uma lista obtida aplicando a função f a cada elemento da lista xs, ou seja, mapFold f xs  == map f xs.
-- mapFold (*2) [1,2,3] == [2,4,6]

mapFold f xs = foldr (\x z -> [f x] ++ z) [] xs

--- removeLista
-- Usando a função foldr, defina a função removeLista tal que (removeLista xs ys) remove todo elemento de ys que ocorre na lista xs.
-- removeLista [1,2] [1,1,3,2,2,4,5] == [3,4,5]

removeLista xs ys = foldr f' [] ys
    where f' z zs = if z `elem` xs then [] ++ zs else [z] ++ zs

--- acertosFold
-- defina uma função f :: [a] -> [a] ->Int tal que (f xs ys i) testa se o i-ésimo elemento da lista xs é igual ao i-ésimo elemento de ys
-- acertosFold "AEDBCCE" "ADDCCBE" == 4
-- acertosFold "ABCDE" "ABCDE" == 5

acertosFold xs ys = foldr g 0 [0..tam-1]
    where
        g i z = if (xs !! i) == (ys !! i) then 1 + z else 0 + z
        tam = length xs 

-- [0..tam-1] list of indexes
-- g          function that checks if both lists' values at an index are the same
--            if they are    it'll add 1 to the score
--            if they aren't it'll add 0 to the score
-- i          current index to be analysed
-- z          current score

--- descompactaFold
-- Usando o foldr, defina  a função descompactaFold ::  [(a, b)] -> ([a], [b]) que transforma uma lista de pares ordenado em um par ordenado onde o primeiro elemento ´e uma lista dos primeiros componentes dos pares ordenados e o segundo elemento é uma lista dos segundos componentes dos pares ordenados.
-- descompactaFold [(1, 2), (3, 4), (5, 6), (4, 5)] == ([1, 3, 5, 4], [2, 4, 6, 5])
-- descompactaFold [(1, 2), (3, 4), (5, 6), (4, 5), (5, 6)] == ([1, 3, 5, 4, 5], [2, 4, 6, 5, 6])

descompactaFold xs = foldr f ([],[]) xs
    where f (x, y) (qs, ws) = (x:qs, y:ws)



main = print (inverteFold [1,2,3])
