import Data.List (isPrefixOf, length)
import Hora (Hora, fromMinutes, fromStr, subtractHours, toMinutes)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

main :: IO ()
main = do
  archivo <- openFile "textfile.txt" ReadMode
  contenido <- hGetContents archivo
  let lineas = lines contenido
      registros = filter (variableFilter ["\"Lunes", "\"Martes", "\"Miercoles", "\"Jueves", "\"Viernes"]) lineas
      struct = structureData registros [""] 0
      people = length (filter ("\"Empleado" `isPrefixOf`) lineas)
      totalHours = snd struct / 60
      avgHoursPerPerson = totalHours / toFloat people
      avgHoursByDay = avgHoursPerPerson / 20
  putStrLn (unlines (fst struct) ++ "\n" ++ show totalHours ++ "\n" ++ show avgHoursPerPerson ++ "\n" ++ show avgHoursByDay)
  hClose archivo

takeField :: Int -> String -> String
takeField n = filter (`notElem` "\"") . (!! n) . words

structureData :: [String] -> [String] -> Float -> ([String], Float {- Esta funcion devuelve un tipo de dato que contiene la lista a mostrar y la suma total de horas trabajadas por todos los empleados, que después operaremos para obtener los resultados que queremos -})
structureData [] s y = (s, y)
structureData w s y = structureData (tail w) (s ++ [formattedString]) (y + toFloat (toMinutes substract))
  where
    substract = purgeHours 2 (head w)
    formattedString = takeField 0 (head w) ++ " " ++ takeField 1 (head w) ++ " " ++ show substract

toFloat :: Int -> Float
toFloat n = read $ show n :: Float

purgeHours :: Int -> String -> Hora
purgeHours i n
  | takeField i n /= "" && takeField (i + 1) n /= "" = subtractHours (fromStr (takeField (i + 1) n)) (fromStr (takeField i n))
  | i /= 10 = purgeHours (i + 2) n
  | otherwise = fromStr "00:00"

variableFilter :: [String] -> String -> Bool {- Esta función se utiliza para filtrar todas las lineas que no sean una fecha -}
variableFilter [] x = False
variableFilter i x = head i `isPrefixOf` x || variableFilter (tail i) x