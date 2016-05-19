{-
Módulo      : Inferencia
Descripción : Reglas de inferencia usadas para las pruebas
Autores     : Alfredo Fanghella, 12-10967
              Ricardo Mena, 12-10872

Aquí se implementan las reglas de inferencia de la lógica ecuacional del 
capítulo 3 del libro de Gries y Schneider. Para ello, se implementan antes
funciones que hacen sustituciones textuales sobre las expresiones.
-}
module Inference where

import Operators
import Theorems

-- Clase cuya única instancia es el tipo Term. Se usa para poder instanciar
-- la clase de las sustituciones sin usar extensiones para el lenguaje.
class IsTerm t where
    getTerm :: t -> Term

instance IsTerm Term where
    getTerm = id

-- Clase para las sustituciones de una sola variable. También se usa para
-- poder instanciar el resto de las sustituciones.
class SimpleSust s where
    getTerms :: s -> (Term, Term)

instance (IsTerm t1, IsTerm t2) => SimpleSust (t1, t2) where
    getTerms (t1, t2) = (getTerm t1, getTerm t2)

-- Clase de tipos que pueden servir para hacer sustituciones textuales
class Sust s where
    showS :: s -> String          -- función para pasar la Sust a String
    sustVar :: Term -> s -> Term  -- caso base de la sustitución textual
    sust :: Term -> s -> Term     -- casos recursivos de la sustitución textual
    sust TrueTerm _ = TrueTerm
    sust FalseTerm _ = FalseTerm
    sust (Not t) s = Not (sust t s)
    sust (BinOp op t1 t2) s = BinOp op (sust t1 s) (sust t2 s)
    sust (Var i) s = sustVar (Var i) s


-- Instancia para la sustitucion simple
instance (IsTerm t1, IsTerm t2) => Sust (t1, t2) where 
    sustVar t (t1, t2) = substitute t (getTerm t1, getTerm t2)
        where substitute (Var i) (t, Var j) = if i == j then t else (Var i)
              substitute t s = error "No se puede sustituir"
    
    showS (t1, t2) = show (getTerm t1) ++ " =: " ++ show (getTerm t2)


-- Instancia para la sustitucion doble
instance (IsTerm t1, SimpleSust s, IsTerm t2) => Sust (t1, s, t2) where
    sustVar t (t1, s, t2) = substitute t (x1, x2, x3, x4)
        where (x1, (x2, x3), x4) = (getTerm t1, getTerms s, getTerm t2) 
              substitute (Var i) (t1, t2, Var j, Var k)
                | k == j = error "No se puede sustituir"
                | i == j = t1
                | i == k = t2
                | otherwise = Var i
              substitute t s = error "No se puede sustituir"
              
    showS (t1, s, t2) = sustitucion
        where term1 = show (getTerm t1)
              term2 = show (getTerm t2)
              sustIni = "(" ++ term1 ++ ", " ++ showS (getTerms s)
              sustFin = ", " ++ term2 ++ ")"
              sustitucion = sustIni ++ sustFin

-- Instancia para la sustitucion triple             
instance (IsTerm t1, IsTerm t2, SimpleSust s, IsTerm t3, IsTerm t4) => Sust (t1, t2, s, t3, t4) where
    sustVar t (t1, t2, s, t3, t4) = substitute t (x1, x2, x3, x4, x5 ,x6)
        where (x1, x2, (x3, x4), x5, x6) = (getTerm t1, getTerm t2, getTerms s, getTerm t3, getTerm t4)
              substitute (Var i) (t1, t2, t3, Var j, Var k, Var h)
                | j == k || j == h || k == h = error "No se puede sustituir"
                | i == j = t1
                | i == k = t2
                | i == h = t3
                | otherwise = Var i
              substitute t s = error "No se puede sustituir"
    
    showS (t1, t2, s, t3, t4) =  sustitucion
        where term1 = show (getTerm t1)
              term2 = show (getTerm t2)
              term3 = show (getTerm t3)
              term4 = show (getTerm t4)
              firstTwo = term1 ++ ", " ++ term2
              lastTwo = term3 ++ ", " ++ term4
              sustIni = "(" ++ firstTwo ++ ", " ++ showS (getTerms s)
              sustFin = ", " ++ lastTwo ++ ")"
              sustitucion = sustIni ++ sustFin

-- Regla de instanciación. Aplica la misma sustitución en ambos lados de una
-- equivalencia y devuelve la nueva ecuación.
instantiate :: (Sust s) => Equation -> s -> Equation
instantiate (Equiv t1 t2) s = (sust t1 s) === (sust t2 s)

-- Regla de Liebniz. Recibe una ecuación y una función (representada con una
-- variable y une expresión que la contiene) y devuelve la ecuación resultante
-- de aplicar la función a ambos lados de la equivalencia original.
liebniz :: Equation -> Term -> Term -> Equation
liebniz (Equiv t1 t2) t (Var z) = (sust t (t1, Var z)) === (sust t (t2, Var z))

-- Regla de inferencia generalizada que engloba tanto la instanciación como
-- la regla de Liebniz. En vez de recibir una ecuación inicial, recibe un 
-- número de teorema.
infer :: (Sust s) => Float -> s -> Term -> Term -> Equation
infer num sus var term = liebniz (instantiate (prop num) sus) term var

-- Ejecución de un paso de una demostración. Esta función infiere una nueva
-- expresión a partir de otra usando las reglas de inferencia.
step :: (Sust s) => Term -> Float -> s -> Term -> Term -> Term
step term num sus var exp
    | term == i1 = i2
    | term == i2 = i1
    | otherwise = error mensaje_error
    where (Equiv i1 i2) = infer num sus var exp
          mensaje_error = mensajeIni ++ mensajeFin
          mensajeIni = "Error al inferir: (" ++ show term
          mensajeFin = ") no es igual a ningún lado de " ++ show (Equiv i1 i2)
     

