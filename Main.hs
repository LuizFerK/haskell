module Main where

import A02

main :: IO ()
main = do putStrLn "Digite um numero: "
          n1 <- readLn
          putStr "Dobro dos numeros digitados: "
          putStrLn (show (dobro n1))