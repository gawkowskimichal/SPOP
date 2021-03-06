module Plansza where

import System.IO
import System.Directory
import Data.Char
import Control.Exception

-- Podstawowe typy
data Bierka = Bierka TypBierki deriving Eq
data TypBierki = Owca | Wilk deriving Eq
type Pole = Maybe Bierka
type Plansza = [[Pole]]
type Pos = (Int, Int)
type Stan = [Pos]
type File_Path = String
data DrzewoStanow = DrzewoStanow {stan::Stan, subds::[DrzewoStanow], sciezka::[Stan]} deriving Show

printInterface::IO()
printInterface = putStr "\nPodaj komende ruchu wilka: 7|9|1|3 \n 7 - góra+lewo \n 9 - góra+prawo \n 1 - dół+lewo \n 3 - dół+prawo \n"

printOptions::IO()
printOptions = putStr "\nPodaj opcję programu: n|z|o|l|q \n n - nowa gra \n z [nazwa_pliku] - zapis gry \n o [nazwa_pliku] - odczyt gry \n l - listowanie plikow w bierzacym katalogu \n q - koniec gry"

printWin::IO()
printWin = do putStr "\n============================================"
              putStr "\n|                GRATULACJE                |"
              putStr "\n|                WILK WYGRAŁ               |"
              putStr "\n============================================"

printLose::IO()
printLose = do putStr "\n============================================"
               putStr "\n|                KONIEC GRY                |"
               putStr "\n|               OWCE WYGRAŁY               |"
               putStr "\n============================================"


printedBoard::Plansza -> String
printedBoard = unlines . map(concatMap printPole)

printBoard::Plansza -> IO()
printBoard a = putStr (printedBoard (a))

printPole::Pole-> String
printPole Nothing = "| --- |"
printPole (Just (Bierka a)) = "|  " ++ show (a) ++ "  |"

instance Show TypBierki where
 show Owca = "O"
 show Wilk = "W"

emptyPole::Pole
emptyPole = Nothing

getPole::Plansza-> Pos-> Pole
getPole board (a, b) = board!!a!!b

updateBoard::Pos-> Pole-> Plansza-> Plansza
updateBoard = updateMatrix

deletePole::Pos-> Plansza-> Plansza
deletePole p = updateBoard p emptyPole

--movePos::Pos-> Pos-> Board-> Board
--movePos p1 p2 b = updateBoard p2 (getPole b p1) (deletePole p1 b)

setPieceOnBoard::TypBierki -> Pos -> Plansza -> Plansza
setPieceOnBoard piece pos b = updateBoard pos (Just (Bierka piece)) b

setPiecesOnBoard::Stan -> Plansza -> Plansza
setPiecesOnBoard a b = setPieceOnBoard Owca (a!!4) (setPieceOnBoard Owca (a!!3) (setPieceOnBoard Owca (a!!2) (setPieceOnBoard Owca (a!!1) (setPieceOnBoard Wilk (head a) b))))

updateList::[a]-> Int-> (a-> a)-> [a]
updateList [] _ _ = []
updateList (x:xs) 0 f = (f x):xs
updateList (x:xs) n f = x:updateList xs (n-1) f

updateMatrix::(Int, Int)-> a-> [[a]]-> [[a]]
updateMatrix (i,j) a m = updateList m i (\z-> updateList z j (const a))

--outside,inside::Pos-> Bool
--outside (a, b) = a < 0 || b < 0 || a > 7 || b > 7
--inside = not . outside

isValidMove :: Int -> Int -> [(Int,Int)]-> Bool
isValidMove row col sheep = if row >= 0 && col >= 0 && row <=7 && col <=7 then
                        not (elem (row, col) sheep)
                      else
                        False

emptyBoard::Plansza

--initialBoard::Board
{--initialBoard = [[Nothing, Just (Bierka Owca), Nothing, Just (Bierka Owca), Nothing, Just (Bierka Owca), Nothing, Just (Bierka Owca)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Just (Bierka Wilk), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]
--}
emptyBoard = [[Nothing|_<- [1..8]]|_<- [1..8]]
--initialState = [(7,0), (0,1), (0,3), (0,5), (0,7)]

askForInitialState :: IO Stan
askForInitialState = do putStrLn ""
                        putStrLn "Podaj liczbe 0, 2, 4, 6 oznaczajaca poczatkowa pozycje Wilka"
                        input <- getLine
                        case input of
                          "0" -> do let initialState = [(7,0), (0,1), (0,3), (0,5), (0,7)]
                                    return initialState
                          "2" -> do let initialState = [(7,2), (0,1), (0,3), (0,5), (0,7)]
                                    return initialState
                          "4" -> do let initialState = [(7,4), (0,1), (0,3), (0,5), (0,7)]
                                    return initialState
                          "6" -> do let initialState = [(7,6), (0,1), (0,3), (0,5), (0,7)]
                                    return initialState
                          otherwise -> do putStrLn "Nieprawidlowa opcja, wybrano wariant domyslny"
                                          let initialState = [(7,0), (0,1), (0,3), (0,5), (0,7)]
                                          return initialState


updateState :: [(Int, Int)] -> Int -> ([(Int, Int)],Bool)
updateState (s:state) move = case move of
                                 7 -> if isValidMove (fst s - 1) (snd s - 1) state then
                                        ([(fst s - 1,snd s - 1), state!!0, state!!1, state!!2, state!!3],True)
                                    else
                                        (s:state,False)
                                 9 -> if isValidMove (fst s - 1) (snd s + 1) state then
                                        ([((fst s) - 1,(snd s) + 1), state!!0, state!!1, state!!2, state!!3],True)
                                    else
                                        (s:state,False)
                                 1 -> if isValidMove (fst s + 1) (snd s - 1) state then
                                        ([((fst s) + 1,(snd s) - 1), state!!0, state!!1, state!!2, state!!3],True)
                                    else
                                        (s:state,False)
                                 3 -> if isValidMove (fst s + 1) (snd s + 1) state then
                                        ([((fst s) + 1,(snd s) + 1), state!!0, state!!1, state!!2, state!!3],True)
                                    else
                                        (s:state,False)


getEmptyBoard :: Plansza
getEmptyBoard = emptyBoard

{--
getInitialState :: Stan
getInitialState = initialState
--}
displayGame :: Stan -> IO()
displayGame a = do printOptions
                   printInterface
                   printBoard (insertStateToBoard a)

inputReader :: Stan -> IO Bool
inputReader currentState = do
          str <- getLine
          if (czyWilkWygrywa currentState) then do printWin
                                                   state <- askForInitialState
                                                   displayGame state
                                                   inputReader state
          else if czyOwceWygrywaja currentState then
               do printLose
                  state <- askForInitialState
                  displayGame state
                  inputReader state
          else do
                  result <- saveStan currentState str
                  if result then do
                          displayGame currentState
                          inputReader currentState
                  else do
                          (currentState,loaded) <- loadStan currentState str
                          if loaded then do
                              displayGame currentState
                              inputReader currentState
                          else do
                              case str of
                                "q" -> return False
                                "l" -> do
                                    listFiles
                                    displayGame currentState
                                    inputReader currentState
                                "7" -> do
                                    putStrLn "góra+lewo"
                                    let (new_state,zmien) = updateState currentState 7
                                    putStrLn (show new_state)
                                    if (czyWilkWygrywa new_state) then do
                                                   printWin
                                                   state <- askForInitialState
                                                   displayGame state
                                                   inputReader state
                                    else if czyOwceWygrywaja new_state then
                                           do printLose
                                              state <- askForInitialState
                                              displayGame state
                                              inputReader state
                                    else do
                                    if zmien then do
                                    let new_state2 = pogonOwce (new_state)
                                    displayGame new_state2
                                    inputReader new_state2
                                    else do
                                    displayGame new_state
                                    inputReader new_state
                                "9" -> do
                                    putStrLn "góra+prawo"
                                    let (new_state,zmien) = updateState currentState 9
                                    putStrLn (show new_state)
                                    if (czyWilkWygrywa new_state) then do
                                                   printWin
                                                   state <- askForInitialState
                                                   displayGame state
                                                   inputReader state
                                    else if czyOwceWygrywaja new_state then
                                           do printLose
                                              state <- askForInitialState
                                              displayGame state
                                              inputReader state
                                    else do
                                    if zmien then do
                                    let new_state2 = pogonOwce (new_state)
                                    displayGame new_state2
                                    inputReader new_state2
                                    else do
                                    displayGame new_state
                                    inputReader new_state
                                "1" -> do
                                    putStrLn "dół+lewo"
                                    let (new_state,zmien) = updateState currentState 1
                                    putStrLn (show new_state)
                                    if (czyWilkWygrywa new_state) then do
                                                   printWin
                                                   state <- askForInitialState
                                                   displayGame state
                                                   inputReader state
                                    else if czyOwceWygrywaja new_state then
                                           do printLose
                                              state <- askForInitialState
                                              displayGame state
                                              inputReader state
                                    else do
                                    if zmien then do
                                    let new_state2 = pogonOwce (new_state)
                                    displayGame new_state2
                                    inputReader new_state2
                                    else do
                                    displayGame new_state
                                    inputReader new_state
                                "3" -> do
                                    putStrLn "dół+prawo"
                                    let (new_state,zmien) = updateState currentState 3
                                    putStrLn (show new_state)
                                    if (czyWilkWygrywa new_state) then do
                                                   printWin
                                                   state <- askForInitialState
                                                   displayGame state
                                                   inputReader state
                                    else if czyOwceWygrywaja new_state then
                                           do printLose
                                              state <- askForInitialState
                                              displayGame state
                                              inputReader state
                                    else do
                                    if zmien then do
                                    let new_state2 = pogonOwce (new_state)
                                    displayGame new_state2
                                    inputReader new_state2
                                    else do
                                    displayGame new_state
                                    inputReader new_state
                                "n" -> do
                                    putStrLn "nowa gra"
                                    state <- askForInitialState
                                    displayGame state
                                    inputReader state
                                otherwise -> do
                                  putStrLn "Niepoprawna komenda."
                                  displayGame currentState
                                  inputReader currentState

saveStan :: Stan -> String -> IO Bool
saveStan s a = if null a then return False else do
                                                      let tokens = words a
                                                      if head tokens == "z" then do
                                                          save s (tokens !! 1)
                                                          putStrLn "Zapis udany!"
                                                          return True
                                                      else
                                                          return False

loadStan :: Stan -> String -> IO (Stan,Bool)
loadStan s a = if null a then return (s,False) else do
                                                      let tokens = words a
                                                      if head tokens == "o" then do
                                                          ns <- try ( load (tokens !! 1)) :: IO (Either SomeException Stan)
                                                          case ns of
                                                            Right stan -> do putStrLn "Odczyt udany!"
                                                                             return (stan,True)
                                                            Left e -> do putStrLn "Odczyt nieudany!!!"
                                                                         return (s,False)
                                                      else
                                                          return (s,False)

save :: Stan -> File_Path -> IO ()
save zs f = writeFile f (show zs)

load :: File_Path -> IO Stan
load f = do
         s <- readFile f
         return (read s)

listFiles :: IO()
listFiles = do
        cd <- getCurrentDirectory
        files <- getDirectoryContents cd
        print files

--initialGame = NowaGra( initialBoard, initialState )

insertStateToBoard :: Stan -> Plansza
insertStateToBoard a = setPiecesOnBoard a emptyBoard


czyWilkWygrywa :: Stan -> Bool
czyWilkWygrywa (x:xs) = do if fst x == 0 then True else False

czyOwceWygrywaja :: Stan -> Bool
czyOwceWygrywaja (s:state) =  if isValidMove (fst s - 1) (snd s - 1) state
                                     || isValidMove (fst s - 1) (snd s + 1) state
                                     || isValidMove (fst s + 1) (snd s - 1) state
                                     || isValidMove (fst s + 1) (snd s + 1) state then
                                     False
                              else True

bliskoscWilkaDoZagrody :: Stan -> Int
bliskoscWilkaDoZagrody (x:xs) =  7 - fst x							
							
bliskoscOwiecDoWilka :: Stan -> Int
bliskoscOwiecDoWilka (x:xs) = 0 - abs (fst x - fst (xs!!0)) - abs (snd x - snd (xs!!0))
						- abs (fst x - fst (xs!!1)) - abs (snd x - snd (xs!!1))
						- abs (fst x - fst (xs!!2)) - abs (snd x - snd (xs!!2))
						- abs (fst x - fst (xs!!3)) - abs (snd x - snd (xs!!3))
rozstrzalOwiec :: Stan -> Int
rozstrzalOwiec (x:xs) = 0 - (abs (fst (xs!!0) - fst (xs!!1))
                                + abs (fst (xs!!0) - fst (xs!!2))
                                + abs (fst (xs!!0) - fst (xs!!3))
                                + abs (fst (xs!!1) - fst (xs!!2))
                                + abs (fst (xs!!1) - fst (xs!!3))
                                + abs (fst (xs!!2) - fst (xs!!3)))

ocenStanWilka :: (Stan,[Stan]) -> Int
ocenStanWilka a = 5 * (bliskoscWilkaDoZagrody (fst a)) + 1 * (bliskoscOwiecDoWilka (fst a))	 + 4 * (rozstrzalOwiec (fst a))					

mozliweRuchyOwiec :: Stan -> Stan -> [Stan]
mozliweRuchyOwiec stanGry [] = []
mozliweRuchyOwiec stanGry (s:polozenieOwiec) = if isValidMove (fst s + 1) (snd s + 1) stanGry
                                                && isValidMove (fst s + 1) (snd s - 1) stanGry
                                            then [(fst s + 1, snd s + 1),(fst s + 1, snd s - 1)] : mozliweRuchyOwiec stanGry polozenieOwiec
                                                else if isValidMove (fst s + 1) (snd s + 1) stanGry
                                                    then [(fst s + 1, snd s + 1)] : mozliweRuchyOwiec stanGry polozenieOwiec
                                                        else if isValidMove (fst s + 1) (snd s - 1) stanGry
                                                            then [(fst s + 1, snd s - 1)] : mozliweRuchyOwiec stanGry polozenieOwiec
                                                                else [] : mozliweRuchyOwiec stanGry polozenieOwiec
															
mozliweStanyOwiec :: Stan -> [Stan]
mozliweStanyOwiec (x:xs) = [[x]++[y]++[xs!!1]++[xs!!2]++[xs!!3] | y <- ((mozliweRuchyOwiec (x:xs) xs) !! 0)]
						   ++ [[x]++[xs!!0]++[y]++[xs!!2]++[xs!!3] | y <- ((mozliweRuchyOwiec (x:xs) xs) !! 1)]
						   ++ [[x]++[xs!!0]++[xs!!1]++[y]++[xs!!3] | y <- ((mozliweRuchyOwiec (x:xs) xs) !! 2)]
						   ++ [[x]++[xs!!0]++[xs!!1]++[xs!!2]++[y] | y <- ((mozliweRuchyOwiec (x:xs) xs) !! 3)]


nastepneStanyWilka :: Stan -> [Pos]
nastepneStanyWilka (x:xs) = [(fst x - 1, snd x - 1),(fst x - 1, snd x + 1), (fst x + 1, snd x - 1), (fst x + 1, snd x + 1)]

mozliweRuchyWilka :: Stan -> [Pos]
mozliweRuchyWilka (x:xs) = [(fst z, snd z) | z <- nastepneStanyWilka (x:xs), isValidMove (fst z) (snd z) xs]

mozliweStanyWilka :: Stan -> [Stan]
mozliweStanyWilka (x:xs) = [ z:xs | z <- mozliweRuchyWilka (x:xs)]

--generacja nastepnego stanu
generujNastepnyPoziom :: Stan -> Int -> [Stan]
generujNastepnyPoziom (x:xs) a = if a == 1 then mozliweStanyWilka (x:xs) else
								 mozliweStanyOwiec (x:xs)
								
--generacja drzewa gry
generujDrzewo :: [Stan] -> Int -> Stan  -> DrzewoStanow
generujDrzewo sciezka 0 stanGry = DrzewoStanow stanGry [] (sciezka++[stanGry])
generujDrzewo sciezka glebokosc stanGry = if czyOwceWygrywaja stanGry || czyWilkWygrywa stanGry then DrzewoStanow stanGry [] (sciezka++[stanGry])
                                    else DrzewoStanow stanGry (map (generujDrzewo (sciezka++[stanGry]) (glebokosc - 1)) ((generujNastepnyPoziom stanGry ((mod (glebokosc - 1) 2))))) (sciezka++[stanGry])
						
isMax :: Int -> [Int] -> Bool
isMax a b = if a == maximum b then True else False

isMin :: Int -> [Int] -> Bool
isMin a b = if a == minimum b then True else False

sameWyniki :: [((Stan,[Stan]), Int)] -> [Int]
sameWyniki a = [snd x | x <- a]

wybierzNajlepszyRuch ::[((Stan,[Stan]),Int)] -> (Stan,[Stan])
wybierzNajlepszyRuch (x:xs) = if isMax (snd x) (sameWyniki (x:xs)) then fst x else wybierzNajlepszyRuch xs

wybierzNajgorszyRuch ::[((Stan,[Stan]),Int)] -> (Stan,[Stan])
wybierzNajgorszyRuch (x:xs) = if isMin (snd x) (sameWyniki (x:xs)) then fst x else wybierzNajlepszyRuch xs

						

wybierzMinMax:: DrzewoStanow-> Int -> ((Stan,[Stan]),Int)
wybierzMinMax (DrzewoStanow  stan [] sciezka) _ = ((stan,sciezka),(ocenStanWilka (stan,sciezka)))
wybierzMinMax (DrzewoStanow  stan ds sciezka) 0 = (wybierzNajlepszyRuch (map (flip wybierzMinMax 1) ds), ocenStanWilka (stan,sciezka))
wybierzMinMax (DrzewoStanow  stan ds sciezka) glebokosc = if mod glebokosc 2 == 0 then (wybierzNajlepszyRuch (map (flip wybierzMinMax (glebokosc + 1)) ds),  ocenStanWilka (stan,sciezka))
													 else (wybierzNajgorszyRuch (map (flip wybierzMinMax (glebokosc + 1)) ds), ocenStanWilka (stan,sciezka))

pogonOwce :: Stan -> Stan
pogonOwce a =(snd (fst (wybierzMinMax (generujDrzewo [] 5 a) 0)))!!1
												
