module Main where

import Plansza

main :: IO ()
main = do
         stanPoczatkowy <- askForInitialState
         displayGame stanPoczatkowy
         fun <- inputReader stanPoczatkowy
         if fun
          then putStrLn "jakas opcja zostala wybrana"
          else do putStrLn "Wyjscie"
                  return()
