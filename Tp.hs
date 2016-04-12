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
frecuenciaTokens = [ \text -> (frecuenciaT t $ cuentas text) / (fromIntegral $ length text) | t <- tokens ]

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
normalizarExtractor ts extractor = \text -> let datosNormalizados = zip ts (normalizar datos) where datos = (map extractor ts) in
                                              snd $ head $ filter (\x -> (fst x) == text) datosNormalizados


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
-- El algoritmo de clasificación que implementaremos es un modelo simple:
-- K-Vecinos Más Cercanos (https://es.wikipedia.org/wiki/K-vecinos_m%C3%A1s_cercanos).
-- La versión que utilizaremos del modelo funciona como se explica a continuación:
--    - Ante una instancia que debe etiquetar, se calcula la distancia a todas las instancias de
--      entrenamiento utilizando alguna medida de distancia definida previamente.
--    - Una vez computadas las distancias, se seleccionan las K instancias más cercanos y se obtiene
--      su etiqueta.
--    - Ante esta lista de etiquetas, se calcula la moda estadı́stica.
--    - Luego, se etiqueta a la nueva instancia con esa moda
--
-- Implementar un clasificador de K-Vecinos más cercanos. knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo .
-- El primer parámetro corresponde al número de vecinos K, luego recibe datos de
-- entrenamiento junto a sus etiquetas y una medida de distancia, devuelve un modelo de predicción.

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas medida = \instancia -> let distancias = map (medida instancia) datos in
                                 moda (kmenores k (zip distancias etiquetas))

moda :: [(Float,Etiqueta)] -> Etiqueta
moda xs = snd (maximum (cuentas (snd (unzip xs))))

kmenores :: Int -> [(Float,Etiqueta)] -> [(Float,Etiqueta)]
kmenores k datos = take k (sort datos)

-- Ejercicio 10
-- Como primer paso, necesitamos separar nuestros datos en datos de entrenamiento y datos de
-- validación, para ello utilizaremos la siguiente función:
-- separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
-- Esta función, toma una matriz de datos xs y sus respectivas etiquetas y, luego dos números n y p,
-- y devuelve el resultado de partir la matriz xs en n particiones dejando la partición número p
-- para validación y el resto para entrenamiento. Es precondición que el número de partición estará
-- entre 1 y n. Para este ejercicio, se debe mantener el orden original en estas particiones.

-- TODO Hacer mas legibles estas funciones. Es inentendible
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = let particiones = (particionar (dropLastElements (zip datos etiquetas) n ) n) in
                                       (xTrain particiones (p-1) , yTrain particiones (p-1), xVal particiones (p-1), yVal particiones (p-1))

particionar :: [(Instancia,Etiqueta)] -> Int -> [[(Instancia,Etiqueta)]]
particionar ls n = let tamano = (div (length ls) n ) in
                        foldr (\x r -> if (length (head r) == tamano) then [[x]] ++ r  else (([x]++(head r)):(tail r)) ) [[]] ls

xTrain :: [[(Instancia,Etiqueta)]] -> Int -> Datos
xTrain ls p  = fst (unzip (concat (eliminarElemento ls p)))

xVal :: [[(Instancia,Etiqueta)]] -> Int -> [Etiqueta]
xVal ls p = snd (unzip (concat (eliminarElemento ls p)))

eliminarElemento :: [a] -> Int -> [a]
eliminarElemento ls i = (take i ls) ++ (drop (i+1) ls)

yTrain :: [[(Instancia,Etiqueta)]] -> Int -> Datos
yTrain ls p = fst (unzip (ls !! p))

yVal :: [[(Instancia,Etiqueta)]] -> Int -> [Etiqueta]
yVal ls p = snd (unzip (ls !! p))

dropLastElements :: [a] -> Int -> [a]
dropLastElements ls n = take (length ls - (mod (length ls) n)) ls

---------------------------------------------------
-- Ejercicio 11
-- Implementar la función accuracy :: [Etiqueta] -> [Etiqueta] -> Float que devuelve la proporción de aciertos de
-- nuestras predicciones sobre las verdaderas etiquetas,

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

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = (foldr (+) 0 (accuracyList n datos etiquetas)) / (fromIntegral n)

accuracyList :: Int -> Datos -> [Etiqueta] -> [Float]
accuracyList n datos etiquetas = [ accuracyP datos etiquetas n p | p <-[1..n] ]

accuracyP :: Datos -> [Etiqueta] -> Int -> Int -> Float
accuracyP datos etiquetas n p = let (x_train, y_train, x_val, y_val ) = (separarDatos datos etiquetas n p) in 
                                  accuracy [(knn 15 x_train x_val distEuclideana y) | y <- y_train] y_val

-- TODO CHECKLIST
-- hacer Tests
-- mejorar ejercicio 10,
-- comentar que hace cada funcion

