module A02 where

dobro x = x + x
quadruplo x = dobro(dobro x)
circleArea r = pi * r^2

sinal :: Int -> Int
sinal n | n < 0     = -1
        | n == 0    = 0
        | otherwise = 1

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst =
  if not (null lst) then
    head lst
  else
    "empty"