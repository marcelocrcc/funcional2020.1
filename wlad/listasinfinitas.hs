{-
c = cycle [1, 2]

-- step 1
-- s !! k states that the last item of s should appear 1 time
-- it does
-- append the next element of c to the list
k = 0
s = [1]
     ^

-- step 2
-- s !! k states that the last item of s should appear 2 times
-- it doesn't, so add one to make it 2 total
-- append the next element of c to the list
k = 1
s = [1, 2]
        ^

-- step 3
-- s !! k states that the last item of s should appear 2 times
-- it doesn't, so add one to make it 2 total
-- append the next element of c to the list
k = 2
s = [1, 2, 2, 1]
           ^
-- step 4
-- s !! k states that the last item of s should appear 2 times
-- it does
-- append the next element of c to the list
k = 3
s = [1, 2, 2, 1, 1, 2]
              ^
-}

--- kolakoski
-- Defina a seguinte lista kolakoski :: [Int] de modo que kolakoski é a sequência de Kolakoski.
-- take 5 kolakoski == [1,2,2,1,1]
-- take 10 kolakoski == [1,2,2,1,1,2,1,2,2,1]
-- take 24 kolakoski == [1,2,2,1,1,2,1,2,2,1,2,2,1,1,2,1,1,2,2,1,2,1,1,2]


--- hamming
-- Defina a seguinte lista hamming :: [Int] de modo que hamming é a sequência de Hamming.
-- take 12 hamming == [1,2,3,4,5,6,8,9,10,12,15,16]
