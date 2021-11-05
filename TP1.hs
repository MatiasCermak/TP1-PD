{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (isPrefixOf, length)
import Hora (Hora, fromStr, subtractHours, toMinutes)
import Language.Haskell.TH (Lit (StringL))
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

main :: IO ()
main = do
  archivo <- openFile "textfile.txt" ReadMode
  contenido <- hGetContents archivo
  let lineas = lines contenido
      registros = filter variableFilter lineas
      struct = ([""], 0, 0)
  putStrLn $ unlines $ structureData registros struct
  hClose archivo

tomarCampo :: Int -> String -> String
tomarCampo n = filter (`notElem` "\"") . (!! n) . words

{- Esta funcion devuelve un tipo de dato que contiene la lista a mostrar y la suma total de
horas trabajadas por todos los empleados, que después operaremos para obtener los resultados que queremos -}
structureData :: [String] -> ([String], Float, Int) -> ([String], Float, Int)
structureData [""] (s, y, z) = (s, y, z)
structureData w (s, y, z) = structureData (tail w) (s ++ [formattedString], y + toFloat (toMinutes substract), z + 1)
  where
    substract = purgeHours $ head w
    formattedString = tomarCampo 0 (head w) ++ " " ++ tomarCampo 1 (head w) ++ " " ++ show substract

toFloat :: Int -> Float
toFloat n = read $ show n :: Float

purgeHours :: String -> Hora
purgeHours n
  | tomarCampo 2 n /= "" && tomarCampo 3 n /= "" = subtractHours (fromStr (tomarCampo 2 n)) (fromStr (tomarCampo 3 n))
  | tomarCampo 4 n /= "" && tomarCampo 5 n /= "" = subtractHours (fromStr (tomarCampo 4 n)) (fromStr (tomarCampo 5 n))
  | tomarCampo 6 n /= "" && tomarCampo 7 n /= "" = subtractHours (fromStr (tomarCampo 6 n)) (fromStr (tomarCampo 7 n))
  | tomarCampo 8 n /= "" && tomarCampo 9 n /= "" = subtractHours (fromStr (tomarCampo 8 n)) (fromStr (tomarCampo 9 n))
  | tomarCampo 10 n /= "" && tomarCampo 11 n /= "" = subtractHours (fromStr (tomarCampo 10 n)) (fromStr (tomarCampo 11 n))
  | otherwise = fromStr "00:00"

{- Esta función se utiliza para filtrar todas las lineas que no sean una fecha -}
variableFilter :: String -> Bool
variableFilter [] = False
variableFilter x
  | "\"Lunes" `isPrefixOf` x = True
  | "\"Martes" `isPrefixOf` x = True
  | "\"Miercoles" `isPrefixOf` x = True
  | "\"Jueves" `isPrefixOf` x = True
  | "\"Viernes" `isPrefixOf` x = True
  | otherwise = False