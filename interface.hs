import System.IO


data State = NowyStan ((Int,Int),(Int,Int),(Int,Int),(Int,Int),(Int,Int)) | Nothing

type FilePath = String

displayState :: IO()

displayInterface :: IO()

saveToFile :: FilePath -> IO()

loadFromFile :: FilePath -> IO()
