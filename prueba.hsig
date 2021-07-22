nroRepeticiones :: Integer -> [Integer] -> Integer --sebas
--Devuelve cuantas veces aparece un numero en una lista.
nroRepeticiones n [] = 0
nroRepeticiones n (x:xs) | n == x = 1 + nroRepeticiones n (xs)
                         | otherwise = nroRepeticiones n (xs)

cualEsElMasRepetido :: [Integer] -> Integer
cualEsElMasRepetido [x] = x
cualEsElMasRepetido (x:xs) | (nroRepeticiones x (x:xs)) > nroRepeticiones (cualEsElMasRepetido xs) xs = x
                           | otherwise = (cualEsElMasRepetido xs)
