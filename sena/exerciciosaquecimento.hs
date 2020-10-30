-- "Aquecimento"
-- https://github.com/senapk/funcional_arcade#aquecimento

--    @004 01. countNeg
countNeg xs = length $ filter (<0) xs

--    @005 01. final
final n xs = drop (length xs - n) xs

--    @002 01. iguais
iguais a b c
    | a == b && a == c           = 3
    | a == b || a == c || b == c = 2
    | otherwise                  = 0

--    @006 01. interior
interior xs = tail $ init xs

--    @008 01. gangorra
gangorra p_1 c_1 p_2 c_2
    | p_1 * c_1 > p_2 * c_2 = -1
    | p_1 * c_1 < p_2 * c_2 = 1
    | otherwise             = 0

--    @010 01. min2
min2 x y
    | x <= y    = x
    | otherwise = y

--    @011 01. min3
--    não checa o y <= x na segunda guarda pois se já passou da primeira o x não é o menor
min3 x y z
    | x <= y && x <= z = x
    | y <= z           = y
    | otherwise        = z

--    @000 01. soma2
soma2 x y = x + y

