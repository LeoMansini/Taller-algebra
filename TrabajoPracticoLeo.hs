import SolucionTP

{- Notacion (--) consigna ({--}) idea-}

--Una sopa de n ́umeroses un juego que consiste en descubrir propiedades de un tablero de n×m 
--(n, m≥1) en los que en cada posicion hay un numero entero positivo.
--Cada posicion se identifica con un par (i,j),
--en el cual la primera componente corresponde a una fila y la segunda a una columna -}

-- *EJ 1*

maximoDeLista :: [Integer] -> Integer
{-Calcula el maximo de una Lista de Enteros.-}
maximoDeLista [x] = x
maximoDeLista (x:xs) = max x (maximoDeLista xs)

listaDeMaximos :: Tablero -> [Integer]
{-Almacena el maximo de cada Fila del Tablero en una lista.-}
listaDeMaximos [f] = [maximoDeLista f]
listaDeMaximos (f:fs) = (maximoDeLista f):(listaDeMaximos fs)

maximo :: Tablero -> Integer
{-Devuelve el numero mas grande de un tablero dado.-}
maximo x = maximoDeLista (listaDeMaximos x)
--Devuelve el numero que mas veces aparece en un tablero dado.
--Si hay empate devuelve cualquiera de ellos.
{-Listar todos los numeros del tablero y ponerlos en orden para facilitar.
Crear tuplas (n, 1) al principio para cada elemento n del tablero. Luego juntar las 
tuplas con el mismo n y sumarles la 2da coordenada. Asi tendremos una lista de (n, r) y
devolveremos el n con un mayor r.-}

-- *EJ 2*

nroRepeticiones :: Integer -> [Integer] -> Integer --sebas
--Devuelve cuantas veces aparece un numero en una lista.
nroRepeticiones n [] = 0
nroRepeticiones n (x:xs) | n == x = 1 + nroRepeticiones n (xs)
                         | otherwise = nroRepeticiones n (xs)

cualEsElMasRepetido :: [Integer] -> Integer
cualEsElMasRepetido [x] = x
cualEsElMasRepetido (x:xs) | (nroRepeticiones x (x:xs)) > nroRepeticiones (cualEsElMasRepetido xs) xs = x
                           | otherwise = (cualEsElMasRepetido xs)

aplanarTablero :: Tablero -> [Integer]
aplanarTablero [] = []
aplanarTablero (x:xs) = x ++ aplanarTablero (xs)
--Toma todas las filas de un Tablero y las concatena.

masRepetido:: Tablero -> Integer
masRepetido xs = cualEsElMasRepetido (aplanarTablero xs)

-- *EJ 3*

valoresDeCamino :: Tablero -> Camino -> [Integer] --sebas
-- Devuelve los valores de las posiciones de un camino.
{-Usar la funcion valor en cada elemento del camino y unirlos todos en una lista.-}
valoresDeCamino tab camino | length camino == 1 && posValida tab (head camino) = [valor tab (head camino)]
                           | posValida tab (head camino) = valor tab (head camino) : valoresDeCamino tab (tail camino)
                           | otherwise = []

-- *EJ 4*

longitud :: [Integer] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

collatzDeLongitudLaux :: Integer -> Integer -> [Integer]
--Construye una sucesion de collatz empezando en el primer parametro y parando de modo que
--tenga una longitud igual al segundo parametro.
collatzDeLongitudLaux an l | an == 1 || l == 1 = []
                          | mod an 2 == 0 = (div an 2) : (collatzDeLongitudLaux (div an 2) (l-1))
                          | otherwise = (3*an+1) : (collatzDeLongitudLaux(3*an+1) (l-1))
collatzDeLongitudL :: Integer -> Integer -> [Integer]
collatzDeLongitudL n l = n:(collatzDeLongitudLaux n l)


caminoDeCollatz :: Tablero -> Camino -> Integer -> Bool
--Toma los valores de un camino en un tablero empezando desde n, luego, verifica si esa parte
--del camino es igual a una sucesion de collatz empezando desde el mismo n y de igual longitud
caminoDeCollatz tab [p] n  = valor tab p == n
caminoDeCollatz tab (pos:poss) n | (valor tab pos) /= n = caminoDeCollatz tab poss n
                                 | otherwise = valoresDeCamino tab (pos:poss) == collatzDeLongitudL n (longitud (valoresDeCamino tab (pos:poss)))

{-Idea para camino de collatz: tomo los valores del camino en el tablero empezando desde el 
3er parametro. Estos valores los almaceno en una lista.
Si no puedo empezar desde el numero pedido o si no termina en 1, descarto el camino. 
Creo otra funcion "collatz" que me de una lista que tenga los terminos de la sucesion empezando en
un numero dado. Luego, comparo collatz(n) con mi posible camino que almacene en una lista, que empieza 
en n y termina en 1. Estas listas deben ser completamente iguales para que sea verdad que ambas son 
camino de collatz.-}

-- *EJ 5*

sigDeCollatz :: Integer -> Integer
-- Devuelve el numero siguiente a n de una sucesión de Collatz iniciada en n
sigDeCollatz n = head (collatzDeLongitudLaux n 2) -- rompe con n = 1

posicionesDeTableroLeoaux :: Tablero -> Integer -> Integer -> [Posicion]
posicionesDeTableroLeoaux tab f c | f > cantidadFilas tab = []
                                  | c > cantidadColumnas tab = posicionesDeTableroLeoaux tab (f+1) 1
                                  | otherwise = (f, c):posicionesDeTableroLeoaux tab f (c+1)

posicionesDeTablero :: Tablero -> [Posicion]
--Devuelve una lista de todas las tuplas de posiciones validas para el tablero
posicionesDeTablero tab = posicionesDeTableroLeoaux tab 1 1

posicionesDeNaux :: Integer -> Tablero -> [Posicion] -> [Posicion]
posicionesDeNaux n tab [] = []
posicionesDeNaux n tab (p:ps) | n == valor tab p  = p:(posicionesDeNaux n tab ps)
                              | otherwise = posicionesDeNaux n tab ps

posicionesDeN :: Integer -> Tablero -> [Posicion]
--Pasa por todas las posiciones del tablero y se fija si el valor de cada posicion es igual a N, si lo es, lo 
--almacena en una lista.
posicionesDeN n tab = posicionesDeNaux n tab (posicionesDeTablero tab)

caminoDeCollatzDesdeN :: Tablero -> Posicion -> Integer
--Se fija desde una posicion, que sera una posicion del N en el tablero, de cuanta longitud es el camino de collatz
--saliendo desde ahi.
caminoDeCollatzDesdeN tab (x, y) | valor tab (x, y) == 1 = 1
                                 | (posValida tab (x+1, y)) && ((sigDeCollatz (valor tab (x, y))) == (valor tab (x+1, y))) && (posValida tab (x, y+1)) && ((sigDeCollatz (valor tab (x, y))) == (valor tab (x, y+1))) = max (1 + caminoDeCollatzDesdeN tab (x+1, y)) (1 + caminoDeCollatzDesdeN tab (x, y+1)) --rarisimo caso en donde para abajo y para la derecha sigue la sucesion de collatz, calculo cuanto mide cada camino para los dos lados y tomo el mayor, total ya voy a tomar el mayor de tooodos los caminos desde todos los N en el tablero.
                                 | (posValida tab (x+1, y)) && ((sigDeCollatz (valor tab (x, y))) == (valor tab (x+1, y))) = 1 + caminoDeCollatzDesdeN tab (x+1, y)
                                 | (posValida tab (x, y+1)) && ((sigDeCollatz (valor tab (x, y))) == (valor tab (x, y+1))) = 1 + caminoDeCollatzDesdeN tab (x, y+1)
                                 | otherwise = 1

caminosDeCollatzDesdeLosNaux :: Tablero -> [Posicion] -> [Integer]
caminosDeCollatzDesdeLosNaux tab [] = []
caminosDeCollatzDesdeLosNaux tab (p:ps) = (caminoDeCollatzDesdeN tab p):(caminosDeCollatzDesdeLosNaux tab ps)

caminosDeCollatzDesdeLosN :: Tablero -> Integer -> [Integer]
--Devuelve las longitudes de todos los caminos de collatz posibles empezando desde N.
caminosDeCollatzDesdeLosN tab n = caminosDeCollatzDesdeLosNaux tab (posicionesDeN n tab)

mayorSecuenciaDeCollatz :: Tablero -> Integer -> [Integer]
--Devuelve una sucesion de collatz que empieza en N y tiene como longitud la maxima longitud de
--todos los caminos de collatz posibles empezando desde N.
mayorSecuenciaDeCollatz tab n | posicionesDeN n tab == [] = []
                              | otherwise = collatzDeLongitudL n (maximoDeLista (caminosDeCollatzDesdeLosN tab n))



{-Idea para mayorSecuenciaDeCollatz: tomo un tablero y me fijo cuales son todas las posiciones del Tablero. Armo una lista
con todas las posiciones del tablero. El tablero tiene una cierta cantidad de Filas y una cierta Cantidad de Columnas. Asi
que todas las posiciones validas seran las que se encuentran en el rango ((1,cantFilas),(1,cantColumnas)). 
Cuando tengo todas las posiciones de un tablero chequeo en cual de esas posiciones hay un N en un tablero.
Cuando consigo las posiciones donde hay un N en el tablero, me fijo si alguno de los dos valores consecutivos a ese n son validos
para formar una secuencia de collatz. Si no lo son, la longitud es 1, si lo son, se le suma 1 a la longitud. Se repite el procedimiento
de fijarse si los siguientes al siguiente pueden formar una sucecion de collatz. si no, la long es 2, si no, se vuelve a repetir el procedimiento
De esta forma, repito el procedimiento hasta que las posiciones no sean validas, si es que no se me corto antes. 
Almaceno en una lista las longitudes de mis posibles caminos de Collatz iniciados en un n dado, y comparo para ver cual es el mayor 
(Con tener las longitudes del camino de Collatz alcanza pues el camino de Collatz desde un cierto n es siempre el mismo, por lo
tanto, con tener la longitud de un Collatz desde cierto n ya sabes cual es el camino de Collatz con esa longitud)-}

--Devuelve la lista de los valores de las posiciones del camino mas largo del tablero de 
--la sucesion Collatz inicializada con el numero positivo pasado como parametro.
--Si hay empate devuelve cualquiera de ellas (seran iguales). 
--Si el numero pasado como parametro no existe en el tablero, se devuelve la lista vacıa. 
--Si el numero esta en el tablero, pero no se puede continuar una sucesion Collatz,  
--devuelve la lista formada solo con el valor del numero. 
--(¡Atencion! Las secuencias de Collatz terminan en el 1. La secuencia [8,4,2,1,4] no es valida)


-- *EJ 6*

filaN :: Tablero -> Integer -> Fila
--Devuelve la n-esima fila de un tablero
filaN (f:fs) 1 = f
filaN (f:fs) n = filaN fs (n-1)

 
{-Empiezo a usar lo de la clase 9-}

vacio :: Conjunto a
vacio = []

pertenece :: Eq a => a -> Conjunto a -> Bool
pertenece _ [] = False
pertenece x (y:cy) = (x == y) || (pertenece x cy)


agregar :: Eq a => a -> Conjunto a -> Conjunto a
agregar x cy | pertenece x cy = cy --por ser un conjunto
             | otherwise = x:cy

union :: Eq a => Conjunto a -> Conjunto a -> Conjunto a
union [] c2 = c2
union (x:c1) c2 = union c1 (agregar x c2)

permutaciones :: Integer -> Conjunto [Integer]
permutaciones 1 = agregar [1] vacio
permutaciones n = insertarEnTodaListaEnTodaPos n (permutaciones (n-1))

insertarEnTodaListaEnTodaPos ::  Integer -> Conjunto [Integer] -> Conjunto [Integer]
insertarEnTodaListaEnTodaPos n (xs:[]) = insertarEnTodaPos n xs
insertarEnTodaListaEnTodaPos n (xs:cxs) = union (insertarEnTodaPos n xs) (insertarEnTodaListaEnTodaPos n cxs)

insertarEnTodaPos :: Integer -> [Integer] -> Conjunto [Integer]
insertarEnTodaPos n xs = insertarEnTodaPosHasta n xs ((longitud xs) + 1)

insertarEnTodaPosHasta :: Integer -> [Integer] -> Integer -> Conjunto [Integer]
insertarEnTodaPosHasta n xs 1 = agregar (n:xs) vacio
insertarEnTodaPosHasta n xs p = agregar (insertarEnPos n xs p) (insertarEnTodaPosHasta n xs (p-1))


insertarEnPos :: Integer -> [Integer] -> Integer -> [Integer]
insertarEnPos n xs 1 = n:xs
insertarEnPos n (x:xs) k = x:(insertarEnPos n xs (k-1))

{-Termina clase 9-}

construirTableroPermutado :: Tablero -> [Integer] -> Tablero
--Con una permutacion de las filas del tablero inicial construyo otro tablero:
--Tomo los numeros de la permutacion como el numero de fila del tablero inicial, asi que armo 
--el tablero usando las filas con el orden que da la permutacion.
construirTableroPermutado tab [] = []
construirTableroPermutado tab (x:xs) = (filaN tab x):(construirTableroPermutado tab xs)

construirTodosLosTablerosaux :: Tablero -> Conjunto [Integer] -> Conjunto Tablero
construirTodosLosTablerosaux tab [] = []
construirTodosLosTablerosaux tab (per:pers) = (construirTableroPermutado tab per):(construirTodosLosTablerosaux tab pers)

construirTodosLosTableros :: Tablero -> Conjunto Tablero
--Construyo un tablero por cada permutacion de las filas del tablero inicial.
construirTodosLosTableros tab = construirTodosLosTablerosaux tab (permutaciones (cantidadFilas tab))

longitudesDeMayoresSecuenciasDeCollatz :: Conjunto Tablero -> Integer -> [Integer]
--Para cada tablero permutado, busco la mayor secuencia de collatz, calculo su longitud,
-- y la almaceno en una lista.
longitudesDeMayoresSecuenciasDeCollatz [] n = []
longitudesDeMayoresSecuenciasDeCollatz (tab:tabs) n = (longitud (mayorSecuenciaDeCollatz tab n)):longitudesDeMayoresSecuenciasDeCollatz tabs n

mayorSecuenciaDeCollatzPermutando :: Tablero -> Integer -> [Integer]
--Tomo la lista de todas las longitudes de los mayores caminos de collatz para cada permutacion del tablero y
--calculo el mayor de esas longitudes, luego devuelvo la sucesion de collatz de esa longitud.
mayorSecuenciaDeCollatzPermutando tab n | m == 0 = [] 
                                        | otherwise = collatzDeLongitudL n m
                                        where m = maximoDeLista (longitudesDeMayoresSecuenciasDeCollatz (construirTodosLosTableros tab) n)


{-"las posibles permutaciones de filas del tablero" significa tomar como elemento a cada fila del
tablero y permutarlas. Si tengo n filas en un tablero tendre n! formas de hacer mi tablero pues
son las formas que tengo de ordenar n elementos de un conjunto de n elementos, pues ademas
todas mis filas del tablero son diferentes por definicion de tablero, y estas
 filas pueden tomarse como n elementos distintos-}

 {- se cuantas combinaciones posibles de tablero hay mediante la funcion "permutaciones"
 tomando como n a la cantidad de elementos de mi tablero (n!)-}

 {- como puedo lograr hacer todas las permutaciones? asigno numeros del 1 al n a mis filas del tablero
 hago "permutaciones n" que me devuelve todas las permutaciones de los numeros del 1 al n Conjunto [Integer]
 como a cada fila le corresponde un numero del 1 al n, cambio cada uno de esos valores por la 
 fila correspondiente. Luego, obtengo todas las posibles permutaciones de filas del tablero-}

 {-asi, obtengo una lista de tableros. Ahora debo aplicar mayor secuencia de Collatz a cada tablero
 posible. hago una funcion que almacene los caminos mas largos de cada tablero, y luego, escribo
 otra funcion que compare las longitudes de estas listas, y devuelva la mas larga de ellas. -}

--Idem ejercicio anterior, devuelve la lista de los valores de las posiciones del camino de Collatz
--mas largo inicializado con el numero positivo pasado como parametro, pero en este caso de todas 
--las permutaciones posibles de filas del tablero.
--Ayuda:se sugiere resolver primero el problema de construir el conjunto de tableros que resulta
--de permutar todas las filas del tablero pasado como parametro y luego llamar a la funcion del 
--ejercicio anterior con cada tablero del conjunto.

{-idea: armo la funcion posibles tableros y almaceno todos los posibles tableros permutados (lista de tableros).
aplico mayorsecuenciadecollatz a cada uno de los posibles tableros. cuando tengo todas las longitudes
observo cual es la mayor y devuelvo el camino de Collatz iniciado en n de esa longitud.-}