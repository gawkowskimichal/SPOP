module Plansza where

import System.IO
import System.Directory


data Piece = Piece PieceType deriving Eq
data PieceType = Owca | Wilk deriving Eq
type Square = Maybe Piece
type Board = [[Square]]
type Pos = (Int, Int)
type Stan = [Pos]
--data Gra = NowaGra (Board, Stan)
type File_Path = String


printInterface::IO()
printInterface = putStr "\nPodaj komende ruchu wilka: 7|9|1|3 \n 7 - góra+lewo \n 9 - góra+prawo \n 1 - dół+lewo \n 3 - dół+prawo \n"

printOptions::IO()
printOptions = putStr "\nPodaj opcję programu: n|z|o|l|q \n n - nowa gra \n z [nazwa_pliku] - zapis gry \n o [nazwa_pliku] - odczyt gry \n l - listowanie plikow w bierzacym katalogu \n q - koniec gry"

printedBoard::Board -> String
printedBoard = unlines . map(concatMap printSquare)

printBoard::Board -> IO()
printBoard a = putStr (printedBoard (a))

printSquare::Square-> String
printSquare Nothing = "| --- |"
printSquare (Just (Piece a)) = "|  " ++ show (a) ++ "  |"

instance Show PieceType where
 show Owca = "O"
 show Wilk = "W"

--isEmpty::Board-> Pos-> Bool
--isEmpty board pos = Nothing == getSquare board pos

emptySquare::Square
emptySquare = Nothing

getSquare::Board-> Pos-> Square
getSquare board (a, b) = board!!a!!b

updateBoard::Pos-> Square-> Board-> Board
updateBoard = updateMatrix

deleteSquare::Pos-> Board-> Board
deleteSquare p = updateBoard p emptySquare

--movePos::Pos-> Pos-> Board-> Board
--movePos p1 p2 b = updateBoard p2 (getSquare b p1) (deleteSquare p1 b)

setPieceOnBoard::PieceType -> Pos -> Board -> Board
setPieceOnBoard piece pos b = updateBoard pos (Just (Piece piece)) b

setPiecesOnBoard::Stan -> Board -> Board
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



emptyBoard::Board

--initialBoard::Board 
{--initialBoard = [[Nothing, Just (Piece Owca), Nothing, Just (Piece Owca), Nothing, Just (Piece Owca), Nothing, Just (Piece Owca)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Just (Piece Wilk), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]
--}
emptyBoard = [[Nothing|_<- [1..8]]|_<- [1..8]]
initialState = [(7,0), (0,1), (0,3), (0,5), (0,7)]

updateState :: [(Int, Int)] -> Int -> [(Int, Int)]
updateState (s:state) move = case move of
                                 7 -> if isValidMove (fst s - 1) (snd s - 1) state then
                                        [(fst s - 1,snd s - 1), (0,1), (0,3), (0,5), (0,7)]
                                    else
                                        s:state
                                 9 -> if isValidMove (fst s - 1) (snd s + 1) state then
                                        [((fst s) - 1,(snd s) + 1), (0,1), (0,3), (0,5), (0,7)]
                                    else
                                        s:state
                                 1 -> if isValidMove (fst s + 1) (snd s - 1) state then
                                        [((fst s) + 1,(snd s) - 1), (0,1), (0,3), (0,5), (0,7)]
                                    else
                                        s:state
                                 3 -> if isValidMove (fst s + 1) (snd s + 1) state then
                                        [((fst s) + 1,(snd s) + 1), (0,1), (0,3), (0,5), (0,7)]
                                    else
                                        s:state


getEmptyBoard :: Board
getEmptyBoard = emptyBoard

getInitialState :: Stan
getInitialState = initialState

displayGame :: Stan -> IO()
displayGame a = do printOptions
                   printInterface
                   printBoard (insertStateToBoard a)


inputReader :: Stan -> IO Bool
inputReader currentState = do
          str <- getLine
          case str of
            "q" -> return False
            "l" -> do
                listFiles
                displayGame currentState
                inputReader currentState
            "7" -> do
                putStrLn "góra+lewo"
                displayGame (updateState currentState 7)
                inputReader (updateState currentState 7)
            "9" -> do
                putStrLn "góra+prawo"
                displayGame (updateState currentState 9)
                inputReader (updateState currentState 9)
            "1" -> do
                putStrLn "dół+lewo"
                displayGame (updateState currentState 1)
                inputReader (updateState currentState 1)
            "3" -> do
                putStrLn "dół+prawo"
                displayGame (updateState currentState 3)
                inputReader (updateState currentState 3)
            "n" -> do
                putStrLn "nowa gra"
                displayGame initialState
                inputReader initialState
            otherwise -> do
              putStrLn "Niepoprawna komenda."
              displayGame currentState
              inputReader currentState

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

insertStateToBoard :: Stan -> Board
insertStateToBoard a = setPiecesOnBoard a emptyBoard
