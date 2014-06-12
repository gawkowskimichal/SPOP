module Main where

import Plansza

main :: IO ()
main = do
         displayGame getInitialState
         fun <- inputReader getInitialState
         if fun
          then putStrLn "jakas opcja zostala wybrana"
          else do putStrLn "Wyjscie"
                  return()
