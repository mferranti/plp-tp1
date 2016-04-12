module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

-- En el archivo Tests-alu.hs se encuentran los ejemplos que exhiben el funcionamiento de las funciones


-- Ejercicio 1
-- 
-- La funcion auxiliar split' agrega elementos a la primer lista del resultado recursivo hasta que se encuentre con el separador.
-- Cuando encuentra el elemento separador se agrega una nueva lista vacia como cabeza y se van agregando los nuevos elementos en
-- esta misma.
-- Luego al resultado de split' se le aplica la funcion eliminarVacios, debido a que en el caso de que hayan varios separadores
-- split' genera listas vacias que deberan ser removidas.


split :: Eq a => a -> [a] -> [[a]]
split a xs = eliminarVacios $ split' a xs

split' :: Eq a => a -> [a] -> [[a]]
split' separador = foldr (\x r ->
                          if (x /= separador)
                          then (x:head r):(tail r)
                          else []:r
                       ) [[]]

eliminarVacios :: [[a]] -> [[a]]
eliminarVacios = filter (\x -> not (null x))

-- Ejercicio 2
--
-- Se obtiene una lista de longitudes de todas las palabras. Si el texto no tiene palabras se devuelve 0 y sino el promedio.

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras = \text -> let longitudes =  map genericLength (split ' ' text) in
                              if null longitudes
                              then 0
                              else mean longitudes

-- Ejercicio 3
--
-- La funcion cuentas' hace una recursion donde en cada paso recursivo chequea si el elemento x ya se encuentra en r
-- si no se encuentra lo agrega como primera aparicion y en caso contrario devuelve la misma lista pero sumando 1
-- a la cantidad del elemento en cuestion.
-- Se agrego la aplicación de reverse para que el orden en el resultado sea el mismo que el orden de aparicion de
-- los elementos.

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = reverse $ cuentas' xs -- se toma reverse para que quede en el orden original de aparicion

cuentas' :: Eq a => [a] -> [(Int,a)]
cuentas' xs = foldr (\x r ->
                    if (elem x (snd (unzip r) ))
                    then map (\y ->
                          if (x == (snd y))
                          then (1 + fst y, snd y)
                          else y
                        )
                        r
                    else (1,x):r
                )
                []
                (reverse xs)

-- Ejercicio 4
--
-- En esta funcion devuelve el resultado de dividir la cantidad de palabras por la cantidad de palabras distintas.

repeticionesPromedio :: Extractor
repeticionesPromedio = \text -> let listPalabras = split ' ' text in
                                  if (null listPalabras)
                                  then 0
                                  else (fromIntegral $ genericLength listPalabras) / (fromIntegral $ genericLength $ cuentas listPalabras)

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"


-- Ejercicio 5

-- La funcion frecuenciaTokens arma una lista por comprension de funciones que devuelven la division entre
-- la cantidad de apariciones del token t dividido la cantidad total de caracteres.
-- En este caso se utiliza la funcion cuentas para obtener la cantidad de apariciones por caracteres.

frecuenciaTokens :: [Extractor]
frecuenciaTokens = [ \text -> (frecuenciaT t (cuentas text)) / fromIntegral (length text) | t <- tokens ]

frecuenciaT :: Char -> [(Int,Char)] -> Feature
frecuenciaT = \t ls -> let k = filter (\x -> t == (snd x)) ls in
                 if (length k == 0)
                 then (fromIntegral 0)
                 else (fromIntegral (fst (head k)))

-- Ejercicio 6

-- En esta funcion lo primero que se hace es normalizar los datos que se obtienen de la aplicacion del extractor
-- a todos los textos. Para normalizar se utiliza la norma infinito (Maximo valor absoluto).
-- Luego se devuelve una funcion que toma un texto (debe estar normalizado) y devuelve el resultado normalizado de
-- la aplicacion del extractor seleccionado.

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor ts extractor = \text -> let datosNormalizados = zip ts (normalizar datos) in
                                              snd ( head ( filter (\x -> (fst x) == text) datosNormalizados))
                                              where datos = (map extractor ts)

normalizar :: [Feature] -> [Feature]
normalizar datos = map (\x -> x / (normaInfinito datos) ) datos

normaInfinito :: [Feature] -> Float
normaInfinito datos = maximum $ map abs datos

norma2 :: [Feature] -> Float
norma2 datos = sqrt (foldr (+) 0 ( map (flip (^) 2) datos))


-- Ejercicio 7

-- Primero se obtiene una lista con extractores normalizados y se guarda en una variable para evitar repetir calculos innecesarios.
-- Luego se devuelve una lista que tiene los resultados de aplicar todos los extractores a cada texto.

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures extractores texts = let extractoresNormalizados = [normalizarExtractor texts ex | ex <- extractores] in
                                      map (\text -> [extractor text | extractor <- extractoresNormalizados] ) texts

-- Ejercicio 8

-- Aplicacion de las formulas matematicas haciendo uso de la funcion zip para tener mas comoda la cuenta dentro de la funcion map.

distEuclideana :: Medida
distEuclideana = \v1 v2 -> sqrt $ foldr (+) 0 $ map (\t -> (fst t - snd t) ^ 2) (zip v1 v2)

distCoseno :: Medida
distCoseno = \v1 v2 -> (foldr (+) 0 $ map (\t -> fst t * snd t) (zip v1 v2)) / ( norma2 v1 * norma2 v2 )

-- Ejercicio 9
--
-- Primero se calcula el vector de distancias de la instancia con respecto a los datos. Este vector mantiene el orden
-- con respecto a la lista de etiquetas.
-- Luego se utiliza la funcion zip para juntar el vector de distancias con la lista de etiquetas para poder ordenar y obtener
-- los k menores sin perder la referencia de las etiquetas.
-- En kmenores se hace uso de la funcion sort para ordenar la lista por el primer elemento de las tuplas obteniendo asi los k-menores
-- elementos en las primeras k posiciones.
--
-- Luego se busca la etiqueta mas comun entre las k etiquetas. En este caso se hace uso de la funcion cuentas que obtendra una lista
-- de 2 elementos: uno sera una tupla con la cantidad de etiquetas I y  el otro una tupla con cantidad de etiquetas F, finalmente se obtiene
-- la mayor de ellas.

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas medir = \instancia -> let distancias = map (medir instancia) datos in
                                 moda (kmenores k (zip distancias etiquetas))

moda :: [(Float,Etiqueta)] -> Etiqueta
moda xs = snd $ maximum $ cuentas $ snd $ unzip xs

kmenores :: Int -> [(Float,Etiqueta)] -> [(Float,Etiqueta)]
kmenores k datos = take k (sort datos)


-- Ejercicio 10
--
-- La funcion separarDatos obtiene una lista de N particiones y la guarda en la variable particiones.
-- Luego hace uso de las funciones xTrain, yTrain, xVal y yVal para obtener las particiones indicadas por P.
-- En vez de pasar P como argumento se pasa (P-1) porque luego se hace uso de la funcion (!!) y esta considera
-- a la posicion 0 como el primer elemento de la lista

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = let particiones = (particionar (dropLastElements (zip datos etiquetas) n ) n) in
                                       (xTrain particiones (p-1) , yTrain particiones (p-1), xVal particiones (p-1), yVal particiones (p-1))

-- Esta funcion obtiene un listado con una cantidad de elementos multiplo de N y devuelve los datos en el mismo orden
-- separados en listas de n elementos.
-- En cada llamado recursivo se chequea si la lista tiene tamaño N, si no lo tiene agrega un Dato mas a esa lista.
-- Si ya alcanzo el tamaño que debe tener esa particion (N) entonces comienza a crear una nueva lista que se pone como cabeza de la
-- la solucion parcial. Y luego sigue agregando Datos en esta lista hasta que llegue al tamaño N.
--
particionar :: [(Instancia,Etiqueta)] -> Int -> [[(Instancia,Etiqueta)]]
particionar ls n = let tamano = (div (length ls) n ) in
                        foldr (\x r -> if (length (head r) == tamano)
                                       then [[x]] ++ r
                                       else (([x]++(head r)):(tail r))
                        )
                        [[]]
                        ls

-- Las funciones xTrai y xVal se encargan de obtener los datos y las etiquetas respectivamente
-- removiendo a la particion P con la funcion eliminarElemento.


xTrain :: [[(Instancia,Etiqueta)]] -> Int -> Datos
xTrain ls p  = fst (unzip (concat (eliminarElemento ls p)))

xVal :: [[(Instancia,Etiqueta)]] -> Int -> [Etiqueta]
xVal ls p = snd (unzip (concat (eliminarElemento ls p)))


-- eliminarElemento elimina el elemento en la posicion i de la lista.
-- Concatena la lista de los elementos anteriores con la lista de los elementos posteriores a dicho indice.

eliminarElemento :: [a] -> Int -> [a]
eliminarElemento ls i = (take i ls) ++ (drop (i+1) ls)

-- Las funciones yTrain y yVal se encargan de obtener los datos y las etiquetas respectivamente
-- de la particion P (P=0 es la primera particion).

yTrain :: [[(Instancia,Etiqueta)]] -> Int -> Datos
yTrain ls p = fst (unzip (ls !! p))

yVal :: [[(Instancia,Etiqueta)]] -> Int -> [Etiqueta]
yVal ls p = snd (unzip (ls !! p))


-- dropLastElements elimina de la lista pasada como argumento los ultimos elementos para que la lista sea particionable
-- en N partes iguales.

dropLastElements :: [a] -> Int -> [a]
dropLastElements ls n = take (length ls - (mod (length ls) n)) ls


-- Ejercicio 11
--
-- Se arma una lista con los pares de etiquetas. Luego se recorre la lista con foldr y se suman todos los matchs.
-- finalmente se divide por la cantidad de etiquetas para obtener la proporcion de aciertos.

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = let list = (zip xs ys) in
                   (fromIntegral (foldr (\x r -> if ((fst x) == (snd x)) then (1+r) else r) 0 list)) / (fromIntegral (length list))

-- Ejercicio 12

-- nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float que calculara el accuracy promedio
-- de nuestro modelo en datos no etiquetados. Para ello, utilizaremos una version de n-fold
-- cross-validation en donde la matriz de datos (y sus respectivas etiquetas) son particionadas en N
-- particiones (siendo N el primer parametro de nuestra funcion).
-- Una vez particionados los datos, seleccionaremos N − 1 particiones para entrenamiento y la
-- restante para validar nuestro modelo. Para este ejercicio el modelo sera: vecinos mas cercanos con
-- K = 15 y la distancia Euclideana como medida.
-- Repetimos este proceso variando cual es la particion seleccionada para validacion entre las N
-- posibles y de esta manera obtenemos N resultados intermedios (accuracy para cada conjunto de
-- validacion). El resultado sera el de promediar estos N resultados intermedios.

-- La funcion nFoldCrossValidation calcula el promedio de todos los accuracy obtenidos para el modelo
-- con el conjunto de datos indicado

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = (foldr (+) 0 (accuracyList n datos etiquetas)) / (fromIntegral n)


-- accuracyList  se encarga de construir una lista de N elementos donde cada elemento
-- es un accuracy de validacion el modelo utilizando una particion de los datos

accuracyList :: Int -> Datos -> [Etiqueta] -> [Float]
accuracyList n datos etiquetas = [ accuracyP datos etiquetas n p | p <-[1..n] ]

-- accuracyP es la funcion encargada de obtener la accuracy del modelo utilizando la particion P

accuracyP :: Datos -> [Etiqueta] -> Int -> Int -> Float
accuracyP datos etiquetas n p = let (x_train, y_train, x_val, y_val ) = (separarDatos datos etiquetas n p) in 
                                  accuracy [(knn 15 x_train x_val distEuclideana y) | y <- y_train] y_val


