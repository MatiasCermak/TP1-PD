data Hora = Hora
  { horas :: Int,
    minutos :: Int
  }

instance Show Hora where
  show (Hora x y)
    | (x < 10) && (y < 10) = "0" ++ show x ++ ":" ++ "0" ++ show y
    | x < 10 = "0" ++ show x ++ ":" ++ show y
    | y < 10 = show x ++ ":" ++ "0" ++ show y
    | otherwise = show x ++ ":" ++ show y

getHoras :: Hora -> Int
getHoras (Hora x _) = x

getMinutos :: Hora -> Int
getMinutos (Hora _ x) = x

toMinutes :: Hora -> Int
toMinutes (Hora x y) = x * 60 + y

fromMinutes :: Int -> Hora
fromMinutes x = Hora (div x 60) (mod x 60)

subtractHours :: Hora -> Hora -> Hora
subtractHours (Hora x y) (Hora a b) = fromMinutes (toMinutes (Hora x y) - toMinutes (Hora a b))

module Horas
(
  Hora,
  getHoras,
  getMinutos,
  toMinutes,
  fromMinutes,
  subtractHours
) where