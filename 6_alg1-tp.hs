import SolucionTP

-----------------------------------------------------------------------
-- TALLER DE ALGEBRA I
-- Verano 2020

-- NÚMERO DE GRUPO: 6

-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1: Mansini, Leo. 318/19.
-- INTEGRANTE 2: Mercado, Aldana Ayelén. 613/19.
-- INTEGRANTE 3: Puglisi, Sebastian. 560/18.
-----------------------------------------------------------------------



-- Ejercicio 1.

maximoDeLista :: [Integer] -> Integer
-- Calcula el máximo número de una lista de enteros.
maximoDeLista [x] = x
maximoDeLista (x:xs) = max x (maximoDeLista xs)

listaDeMaximos :: Tablero -> [Integer]
-- Almacena el maximo número de cada Fila del Tablero en una lista de enteros.
listaDeMaximos [f] = [maximoDeLista f]
listaDeMaximos (f:fs) = (maximoDeLista f):(listaDeMaximos fs)

maximo :: Tablero -> Integer
-- Devuelve el número más grande de un Tablero dado.
maximo x = maximoDeLista (listaDeMaximos x)



-- Ejercicio 2.

nroRepeticiones :: Integer -> [Integer] -> Integer 
-- Devuelve la cantidad de veces que aparece un número en una lista.
nroRepeticiones n [] = 0
nroRepeticiones n (x:xs) | n == x = 1 + nroRepeticiones n (xs)
                         | otherwise = nroRepeticiones n (xs)

cualEsElMasRepetido :: [Integer] -> Integer
-- Devuelve el entero que más se repite en una lista.
cualEsElMasRepetido [x] = x
cualEsElMasRepetido (x:xs) | (nroRepeticiones x (x:xs)) > nroRepeticiones (cualEsElMasRepetido xs) xs = x
                           | otherwise = (cualEsElMasRepetido xs)

aplanarTablero :: Tablero -> [Integer]
-- Toma las Filas de un Tablero y las concatena.
aplanarTablero [] = []
aplanarTablero (x:xs) = x ++ aplanarTablero (xs)

masRepetido:: Tablero -> Integer
-- Devuelve el número que más veces aparece en un Tablero dado. Si hay empate devuelve cualquiera de ellos.
-- Funciona aplanando el Tablero, es decir, tomando todos los valores del Tablero y poniéndolos en una lista,
-- luego, se calcula el más repetido de esa lista.
masRepetido xs = cualEsElMasRepetido (aplanarTablero xs)



-- Ejercicio 3.

valoresDeCamino :: Tablero -> Camino -> [Integer] 
-- Devuelve los valores de las Posiciones de un Camino en un Tablero dado.
valoresDeCamino tab camino | length camino == 1 && posValida tab (head camino) = [valor tab (head camino)]
                           | posValida tab (head camino) = valor tab (head camino) : valoresDeCamino tab (tail camino)
                           | otherwise = []



-- Ejercicio 4.

longitud :: [Integer] -> Integer
-- Devuelve el valor de la longitud de una lista de enteros.
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

collatzDeLongitudLaux :: Integer -> Integer -> [Integer]
-- Construye una sucesión de Collatz que comienza en el número siguiente al primer parámetro y se
-- detiene cuando su longitud es igual al segundo parámetro.
collatzDeLongitudLaux an l | an == 1 || l == 1 = []
                           | mod an 2 == 0 = (div an 2) : (collatzDeLongitudLaux (div an 2) (l-1))
                           | otherwise = (3*an+1) : (collatzDeLongitudLaux(3*an+1) (l-1))

collatzDeLongitudL :: Integer -> Integer -> [Integer]
-- Esta función agrega el valor inicial de la sucesión de Collatz.
collatzDeLongitudL n l = n:(collatzDeLongitudLaux n l)

caminoDeCollatz :: Tablero -> Camino -> Integer -> Bool
-- Con un Tablero dado, toma los valores de un Camino que esten después de un n. Luego, verifica si esa parte
-- del Camino es igual a una sucesión de Collatz de igual longitud y que comienza en el mismo n.
caminoDeCollatz tab [p] n  = valor tab p == n
caminoDeCollatz tab (pos:poss) n | (valor tab pos) /= n = caminoDeCollatz tab poss n
                                 | otherwise = valoresDeCamino tab (pos:poss) == collatzDeLongitudL n (longitud (valoresDeCamino tab (pos:poss)))



-- Ejercicio 5.

sigDeCollatz :: Integer -> Integer
-- Devuelve el consecutivo de n en una sucesión de Collatz.
sigDeCollatz n = head (collatzDeLongitudLaux n 2)

posicionesDeTableroaux :: Tablero -> Integer -> Integer -> [Posicion]
-- Devuelve una lista de todas las Posiciones de un Tablero comenzando en la Posición (f,c).
posicionesDeTableroaux tab f c | f > cantidadFilas tab = []
                               | c > cantidadColumnas tab = posicionesDeTableroaux tab (f+1) 1
                               | otherwise = (f, c):posicionesDeTableroaux tab f (c+1)

posicionesDeTablero :: Tablero -> [Posicion]
-- Devuelve una lista de todas las tuplas de Posiciones válidas para el Tablero.
posicionesDeTablero tab = posicionesDeTableroaux tab 1 1

posicionesDeNaux :: Integer -> Tablero -> [Posicion] -> [Posicion]
-- Tomando las posiciones dadas, almacena solo las Posiciones cuyo valor en un Tablero son n.
posicionesDeNaux n tab [] = []
posicionesDeNaux n tab (p:ps) | n == valor tab p  = p:(posicionesDeNaux n tab ps)
                              | otherwise = posicionesDeNaux n tab ps

posicionesDeN :: Integer -> Tablero -> [Posicion]
-- Pasa por todas las Posiciones del Tablero y se fija si el valor de cada Posición es igual a n, si lo es, lo 
-- almacena en una lista.
posicionesDeN n tab = posicionesDeNaux n tab (posicionesDeTablero tab)

caminoDeCollatzDesdeN :: Tablero -> Posicion -> Integer
-- Devuelve la longitud del Camino de Collatz que sale desde una Posición de n en el Tablero dado.
-- La segunda guarda contempla el caso en el que desde una Posición la sucesion de Collatz continúa tanto
-- para la derecha como para abajo. En ese caso, hay dos posibles Caminos de Collatz, pero la función solo devuelve
-- la longitud del mas largo de los dos.
caminoDeCollatzDesdeN tab (x, y) | valor tab (x, y) == 1 = 1
                                 | (posValida tab (x+1, y)) && ((sigDeCollatz (valor tab (x, y))) == (valor tab (x+1, y))) && (posValida tab (x, y+1)) && ((sigDeCollatz (valor tab (x, y))) == (valor tab (x, y+1))) = max (1 + caminoDeCollatzDesdeN tab (x+1, y)) (1 + caminoDeCollatzDesdeN tab (x, y+1))
                                 | (posValida tab (x+1, y)) && ((sigDeCollatz (valor tab (x, y))) == (valor tab (x+1, y))) = 1 + caminoDeCollatzDesdeN tab (x+1, y)
                                 | (posValida tab (x, y+1)) && ((sigDeCollatz (valor tab (x, y))) == (valor tab (x, y+1))) = 1 + caminoDeCollatzDesdeN tab (x, y+1)
                                 | otherwise = 1

caminosDeCollatzDesdeLosNaux :: Tablero -> [Posicion] -> [Integer]
-- Devuelve las longitudes de los Caminos de Collatz pertenecientes a un Tablero
-- que comienzan en las Posiciones tomadas como parámetro.
caminosDeCollatzDesdeLosNaux tab [] = []
caminosDeCollatzDesdeLosNaux tab (p:ps) = (caminoDeCollatzDesdeN tab p):(caminosDeCollatzDesdeLosNaux tab ps)

caminosDeCollatzDesdeLosN :: Tablero -> Integer -> [Integer]
-- Devuelve las longitudes de todos los Caminos de Collatz posibles empezando desde n.
-- Esta función sirve para que si hay mas de un n en un Tablero, se contemplen Caminos de Collatz 
-- saliendo de todos los n's en el Tablero.
caminosDeCollatzDesdeLosN tab n = caminosDeCollatzDesdeLosNaux tab (posicionesDeN n tab)

mayorSecuenciaDeCollatz :: Tablero -> Integer -> [Integer]
-- Devuelve una sucesión de Collatz que empieza en n y tiene como longitud la maxima longitud de
-- todos los Caminos de Collatz posibles en un Tablero empezando desde n.
mayorSecuenciaDeCollatz tab n | posicionesDeN n tab == [] = []
                              | otherwise = collatzDeLongitudL n (maximoDeLista (caminosDeCollatzDesdeLosN tab n))



-- Ejercicio 6.

-- Para realizar las permutaciones se utilizará el tipo: "Conjunto", el cual consideraremos que no tiene elementos
-- repetidos, y en donde el orden de los elementos no cambia el Conjunto.

vacio :: Conjunto a
-- Función que representa al Conjunto vacío.
vacio = []

pertenece :: Eq a => a -> Conjunto a -> Bool
-- Función que dado un elemento y un Conjunto retorna verdadero si el elemento pertenece al Conjunto.
pertenece _ [] = False
pertenece x (y:cy) = (x == y) || (pertenece x cy)

agregar :: Eq a => a -> Conjunto a -> Conjunto a
-- Agrega un elemento a un Conjunto.
agregar x cy | pertenece x cy = cy 
             | otherwise = x:cy

union :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
-- Retorna la unión de dos Conjuntos.
union [] c2 = c2
union (x:c1) c2 = union c1 (agregar x c2)

insertarEnPos :: Integer -> [Integer] -> Integer -> [Integer]
-- Devuelve una lista en donde se insertó n en la Posición k de la lista (x:xs) y los elementos siguientes corridos en una Posición.
insertarEnPos n xs 1 = n:xs
insertarEnPos n (x:xs) k = x:(insertarEnPos n xs (k-1))

insertarEnTodaPosHasta :: Integer -> [Integer] -> Integer -> Conjunto [Integer]
-- Devuelve un Conjunto de lista en donde se insertó n en cada Posición de la lista tomada como parámetro 
-- hasta la Posición p.
insertarEnTodaPosHasta n xs 1 = agregar (n:xs) vacio
insertarEnTodaPosHasta n xs p = agregar (insertarEnPos n xs p) (insertarEnTodaPosHasta n xs (p-1))

insertarEnTodaPos :: Integer -> [Integer] -> Conjunto [Integer]
-- Devuelve un Conjunto de lista en donde se insertó n en cada posible Posición de la lista tomada como parámetro.
insertarEnTodaPos n xs = insertarEnTodaPosHasta n xs ((longitud xs) + 1)

insertarEnTodaListaEnTodaPos ::  Integer -> Conjunto [Integer] -> Conjunto [Integer]
-- Devuelve el Conjunto de listas que tiene todas las listas obtenidas de insertar n en todas las Posiciones posibles 
-- en todas las listas del Conjunto.
insertarEnTodaListaEnTodaPos n (xs:[]) = insertarEnTodaPos n xs
insertarEnTodaListaEnTodaPos n (xs:cxs) = union (insertarEnTodaPos n xs) (insertarEnTodaListaEnTodaPos n cxs)

permutaciones :: Integer -> Conjunto [Integer]
-- Genera todas las permutaciones de los números del 1 al n. Cada permutación es devuelta como una lista,
-- y a su vez todas las permutaciones se agregan a una lista. 
permutaciones 1 = agregar [1] vacio
permutaciones n = insertarEnTodaListaEnTodaPos n (permutaciones (n-1))

filaN :: Tablero -> Integer -> Fila
-- Devuelve la n-ésima Fila de un Tablero.
filaN (f:fs) 1 = f
filaN (f:fs) n = filaN fs (n-1)

construirTableroPermutado :: Tablero -> [Integer] -> Tablero
-- Tomando un Tablero, y una lista de enteros, se crea otro Tablero que tendrá las mismas Filas que
-- el primer Tablero, pero ordenadas de la manera que diga la lista dada, siendo el número n en la lista
-- la n-esima Fila del Tablero dado.
construirTableroPermutado tab [] = []
construirTableroPermutado tab (x:xs) = (filaN tab x):(construirTableroPermutado tab xs)

construirTodosLosTablerosaux :: Tablero -> Conjunto [Integer] -> Conjunto Tablero
-- Dado un Tablero y un Conjunto de lista de enteros, construye un Conjunto de Tableros según 
-- el orden de cada lista dentro del Conjunto de la lista de enteros.
construirTodosLosTablerosaux tab [] = []
construirTodosLosTablerosaux tab (per:pers) = (construirTableroPermutado tab per):(construirTodosLosTablerosaux tab pers)

construirTodosLosTableros :: Tablero -> Conjunto Tablero
-- Con un Tablero dado genero todas las permutaciones de la lista [1, 2, .., cantidadFilas Tablero]. Con
-- cada permutacion creo un Tablero utilizando la funcion construirTableroPermutado.
-- Almaceno estos Tableros en un Conjunto de Tableros.
construirTodosLosTableros tab = construirTodosLosTablerosaux tab (permutaciones (cantidadFilas tab))

longitudesDeMayoresSecuenciasDeCollatz :: Conjunto Tablero -> Integer -> [Integer]
-- Para cada Tablero permutado, busco la mayor secuencia de Collatz, y almaceno su longitud en una lista.
longitudesDeMayoresSecuenciasDeCollatz [] n = []
longitudesDeMayoresSecuenciasDeCollatz (tab:tabs) n = (longitud (mayorSecuenciaDeCollatz tab n)):longitudesDeMayoresSecuenciasDeCollatz tabs n

mayorSecuenciaDeCollatzPermutando :: Tablero -> Integer -> [Integer]
-- Tomo la lista de todas las longitudes de los mayores Caminos de Collatz para cada permutacion del Tablero y
-- calculo la mayor de esas longitudes, luego devuelvo la sucesion de Collatz de esa longitud.
mayorSecuenciaDeCollatzPermutando tab n | m == 0 = [] 
                                        | otherwise = collatzDeLongitudL n m
                                        where m = maximoDeLista (longitudesDeMayoresSecuenciasDeCollatz (construirTodosLosTableros tab) n)


                                        