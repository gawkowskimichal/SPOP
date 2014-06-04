module Main where

import Plansza

main :: IO ()
main = do
         displayGame
         fun <- inputReader
         if fun
          then putStrLn "jakas opcja zostala wybrana"
          else do putStrLn "Wyjscie"
                  return()
