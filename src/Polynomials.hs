module Polynomials where

-- (a)
optimalX :: (Fractional a, Eq a) => a -> a-> a -> a
optimalX a b c = 
  if a == 0
  then error "a must not be 0"
  else -b / (2 * a) -- (ax^2 + bx + c)' = 2ax + b = 0  <==>  x = -b / (2*a)
  
-- (b)
optimalY :: (Fractional a, Eq a) => a -> a -> a -> a
optimalY a b c = 
  let x = optimalX a b c 
  in a * x^2 + b * x + c
  
-- (c)
optimal :: (Fractional a, Eq a) => a -> a -> a -> (a, a)
optimal a b c = (optimalX a b c, optimalY a b c)   

