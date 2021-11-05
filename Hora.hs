module Hora
  ( Hora,
    toMinutes,
    fromMinutes,
    subtractHours,
    fromStr,
  )
where

data Hora = Hora {horas :: Int, minutos :: Int}

instance Show Hora where
  show (Hora x y)
    | (x < 10) && (y < 10) = "0" ++ show x ++ ":" ++ "0" ++ show y
    | x < 10 = "0" ++ show x ++ ":" ++ show y
    | y < 10 = show x ++ ":" ++ "0" ++ show y
    | x == -1 && y == -1 = "00:00"
    | otherwise = show x ++ ":" ++ show y

fromStr :: [Char] -> Hora
fromStr "" = Hora (-1) (-1)
fromStr n = Hora (read $ takeWhile (/= ':') n :: Int) (read $ reverse $ takeWhile (/= ':') $ reverse n :: Int)

toMinutes :: Hora -> Int
toMinutes (Hora (-1) (-1)) = -1
toMinutes (Hora x y) = x * 60 + y

fromMinutes :: Int -> Hora
fromMinutes x = Hora (div x 60) (mod x 60)

subtractHours :: Hora -> Hora -> Hora
subtractHours (Hora x y) (Hora a b) = fromMinutes (toMinutes (Hora x y) - toMinutes (Hora a b))