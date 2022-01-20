tx = ((*) ((+) 3 4) ((+) 7 8))

add2 x y k = k ((+) x y)
mul2 x y k = k ((*) x y)

ty = add2 3 4 (\x -> add2 7 8 (\y -> mul2 x y id))
