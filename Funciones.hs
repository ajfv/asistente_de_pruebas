module Funciones where

import Operators
import ProofAssistant
import Theorems

instantiate :: (Sust s) => Equation -> s -> Equation
instantiate (Equiv t1 t2) s = (sust t1 s) === (sust t2 s)

liebniz :: Equation -> Term -> Term -> Equation
liebniz (Equiv t1 t2) t (Var z) = (sust t (t1 =: (Var z))) === (sust t (t2 =: (Var z)))

infer :: (Sust s) => Float -> s -> Term -> Term -> Equation
infer num sus var term = liebniz (instantiate (prop num) sus) term var

step :: (Sust s) => Term -> Float -> s -> Term -> Term -> Term
step term num sus var exp
    | term == i1 = i2
    | term == i2 = i1
    | otherwise = error mensaje_error
    where (Equiv i1 i2) = infer num sus var exp
          mensaje_error = mensajeIni ++ mensajeFin
          mensajeIni = "Error al inferir: (" ++ show term
          mensajeFin = ") no es igual a ningún lado de " ++ show (Equiv i1 i2)
     
-- Constantes auxiliares para la llamada a la funcion statement    
with = ()
using = ()
lambda = ()

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
   
-- Funciones para el inicio y el fin de una demostracion 
proof :: Equation -> IO Term
proof (Equiv t1 t2) = do
    putStrLn $ show t1
    return t1
    
done :: Equation -> Term -> IO()
done (Equiv t2 t3) t1 = do
    if t1 == t3
      then
      putStrLn $ "La demostracion del teorema fue exitosa"
    else
      putStrLn $ "La demostracion del teorema no esta correcta"
    
