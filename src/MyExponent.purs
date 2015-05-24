module MyExponent where

even :: Number -> Boolean
even n = n % 2 == 0

exp :: Number -> Number -> Number
exp _ 0           =  1
exp x n | n > 0   =  f x (n-1) x where
  f _ 0 y = y
  f a d y = g a d  where
    g b i | even i  = g (b*b) (i/2)
          | otherwise = f b (i-1) (b*y)
  exp _ _           = 0

