{-
Módulo      : ProofAssitant
Descripción : Asistente de pruebas de lógica proposicional
Autores     : Alfredo Fanghella, 12-10967
              Ricardo Mena, 12-10872

Este módulo implementa las funciones que son visibles para el usuario y 
muestran sálida. Además exporta los operadores y las variables. Para verificar
una prueba, el usuario solo debe importar este módulo.
-}
module ProofAssistant 
( module ProofAssistant
, module Variables
, false, true, neg, (/\), (\/), (==>), (<==>), (!<==>), (===)
) where

import Operators
import Variables
import Inference
import Theorems

-- Operador para definir una sustitución.
infix 1 =:
(=:) :: Term -> Term -> (Term, Term)
t1 =: t2 = (t1, t2)

-- Constantes auxiliares para que la función statement luzca similar
-- a como se vería si la prueba se hiciera a mano. 
with = ()
using = ()
lambda = ()

-- Esta función ejecuta un paso de la demostración, mostrando la inferencia
-- realizada por la salida estándar.
-- Recibe un número de teorma, la sustitución con la que se instanciará, la
-- variable y la función con que se aplicará la regla de Liebniz y
-- el término actual de la demostración. Si el paso es correcto, devuelve el 
-- siguiente término.
statement :: (Sust s) => Float -> () -> s -> () -> () -> Term -> Term -> Term -> IO Term
statement num _ sus _ _ var exp term = do
    let newTerm = step term num sus var exp
    putStrLn $ statement
    putStrLn $ show newTerm
    return newTerm
    where statement = teorema ++ sustitucion ++ lambda
          teorema = "=== <statement " ++ show num
          sustitucion = " with "++ showS sus
          lambda = " using lambda " ++ show var ++ "." ++ show exp
   
-- Función para iniciar una demostración.
proof :: Equation -> IO Term
proof (Equiv t1 t2) = do
    putStrLn $ show t1
    return t1

-- Función para terminar una demostración. Chequea que se haya llegado a la
-- expresión deseada.
done :: Equation -> Term -> IO()
done (Equiv t2 t3) t1 = do
    if t1 == t3
      then
      putStrLn $ "La demostracion del teorema fue exitosa"
    else
      putStrLn $ "La demostracion del teorema no esta correcta"
    

