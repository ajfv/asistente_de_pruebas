module ProofAssistant where

import Operators

infix 1 =:
(=:) :: Term -> Term -> (Term, Term)
t1 =: t2 = (t1, t2)

class IsTerm t where
    getTerm :: t -> Term

instance IsTerm Term where
    getTerm = id

class SimpleSust s where
    getTerms :: s -> (Term, Term)

instance (IsTerm t1, IsTerm t2) => SimpleSust (t1, t2) where
    getTerms (t1, t2) = (getTerm t1, getTerm t2)

class Sust s where
    sustVar :: Term -> s -> Term
    sust :: Term -> s -> Term
    sust TrueTerm _ = TrueTerm
    sust FalseTerm _ = FalseTerm
    sust (Not t) s = Not (sust t s)
    sust (BinOp op t1 t2) s = BinOp op (sust t1 s) (sust t2 s)
    sust (Var i) s = sustVar (Var i) s

instance (IsTerm t1, IsTerm t2) => Sust (t1, t2) where 
    sustVar t (t1, t2) = substitute t (getTerm t1, getTerm t2)
        where substitute (Var i) (t, Var j) = if i == j then t else (Var i)
              substitute t s = error "No se puede sustituir"

instance (IsTerm t1, SimpleSust s, IsTerm t2) => Sust (t1, s, t2) where
    sustVar t (t1, s, t2) = substitute t (x1, x2, x3, x4)
        where (x1, (x2, x3), x4) = (getTerm t1, getTerms s, getTerm t2) 
              substitute (Var i) (t1, t2, Var j, Var k)
                | k == j = error "No se puede sustituir"
                | i == j = t1
                | i == k = t2
                | otherwise = Var i
              substitute t s = error "No se puede sustituir"

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

instantiate :: (Sust s) => Equation -> s -> Equation
instantiate (Equiv t1 t2) s = (sust t1 s) === (sust t2 s)

liebniz :: Equation -> Term -> Term -> Equation
liebniz (Equiv t1 t2) t (Var z) = (sust t (t1 =: (Var z))) === (sust t (t2 =: (Var z)))


