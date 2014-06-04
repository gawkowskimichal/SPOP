import System.IO

type Board = [[Square]]
type Pos = (Int, Int)
data State = NowyStan (Pos,Pos,Pos,Pos,Pos) | Nothing
data Piece = Piece PieceType deriving Eq
data PieceType = Owca | Wilk deriving Eq

type FilePath = String

printBoard::Board->String
printBoard  = unlines . map (concatMap prettySquare)

printSquare::Square->String
printSquare Nothing = "-- "
printSquare (Just (Piece a)) = show a ++ " "

instance Show PieceType where
 show Owca = "O"
 show Wilk = "W"

isEmpty::Board->Pos->Bool
isEmpty board pos = Nothing == getSquare board pos

emptySquare::Square
emptySquare = Nothing

getSquare::Board->Pos->Square
getSquare board (a, b) = board!!a!!b

updateBoard::Pos-> Square-> Board-> Board
updateBoard = updateMatrix

deleteSquare::Pos->Board->Board
deleteSquare p = updateBoard p emptySquare

movePos::Pos->Pos->Board->Board
movePos p1 p2 b = updateBoard p2 (getSquare b p1) (deleteSquare p1 b)

outside,inside::Pos->Bool
outside (a, b) = a < 0 || b < 0 || a > 7 || b > 7
inside = not . outside

initialBoard, emptyBoard::Board
initialBoard = [[Nothing, Just (Piece Owca), Nothing, Just (Piece Owca), Nothing, Just (Piece Owca), Nothing, Just (Piece Owca)],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
                [Just (Piece Wilk), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

emptyBoard = [[Nothing|_<-[1..8]]|_<-[1..8]]

displayState :: IO()

displayInterface :: IO()

saveToFile :: FilePath -> IO()

loadFromFile :: FilePath -> IO()
