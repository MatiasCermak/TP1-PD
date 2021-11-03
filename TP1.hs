{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List (isPrefixOf, length)
import Data.String (IsString (fromString))
import Data.Text.Internal.Builder (fromString)
import Hora
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

main :: IO ()
main = do
  archivo <- openFile "textfile.txt" ReadMode
  contenido <- hGetContents archivo
  let lineas = lines contenido
      registros = filter variableFilter lineas
  putStrLn $ unlines $ map structureData registros
  hClose archivo

tomarCampo :: Int -> String -> String
tomarCampo n = filter (`notElem` "\"") . (!! n) . words

{- Esta funcion devuelve un tipo de dato que contiene la lista a mostrar y la suma total de
horas trabajadas por todos los empleados, que después operaremos para obtener los resultados que queremos -}
structureData :: String -> ([String], Int)

structureData n

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