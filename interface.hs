import System.IO


data State = NowyStan ((Int,Int),(Int,Int),(Int,Int),(Int,Int),(Int,Int)) | Nothing

type FilePath = String

displayState :: IO()

displayInterface :: IO()

saveToFile :: FilePath -> IO()

loadFromFile :: FilePath -> IO()


type Board = Array Int Piece

data Piece = Piece PieceType

data PieceType = Owca | Wilk



emptyBoard :: Board
emptyBoard = array (0,63) []

piece :: Char -> Piece
piece c = Piece pType
pType = case (toUpper c) of
'O' -> Owca
'W' -> Wilk

addToBoard :: Piece -> Int -> Board -> Board
addToBoard piece loc board = board
