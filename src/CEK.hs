{-|
Module      : CEK
Description : 
Copyright   : (c) Fausto Di Domenico, Federico Stizza, 2020.
License     : GPL-3
Maintainers  : fstizza@fceia.unr.edu.ar, fdidomenico@fceia.unr.edu.ar
Stability   : experimental
-}

module CEK (evalCEK) where

import Lang
import Common
import Subst

import MonadPCF

-- | Valores de la máquina CEK.
data CVal = CEKNat Int | CEKCl Clos deriving (Show)
data Clos = ClosFun CEnv ClosEnv Term | ClosFix CEnv ClosEnv ClosEnv Term deriving (Show)

type ClosEnv = (Name,Ty)

-- | Valores de la máquina CEK.
type CEnv = [CVal]

data Frame =
 KArg CEnv Term                 -- ρ · [] t 
 | KClos Clos                   -- clos []
 | KCond CEnv Term Term         -- ρ · ifz [] then t else e
 | KBinary1 BinaryOp CEnv Term  -- binary [] t
 | KBinary2 BinaryOp CVal       -- binary v []
 deriving (Show)
 
type Kont = [Frame]

search :: MonadPCF m => Term -> CEnv -> Kont -> m CVal
search (BinaryOp _ op t t') p k = search t p (KBinary1 op p t':k)
search (IfZ _ c t e)      p k = search c p ((KCond p t e):k)
search (App _ t u)        p k = search t p ((KArg p u):k)
search (Const _ (CNat n)) _ k = destroy (CEKNat n) k
search (Lam _ n ty t)      p k = destroy (CEKCl (ClosFun p (n,ty) t)) k
search (Fix _ f fty n nty t)  p k = destroy (CEKCl (ClosFix p (f,fty) (n,nty) t)) k
search (V _ (Bound x))    p k = destroy (p!!x) k
search (V _ (Free v))     p k = do v' <- lookupDecl v
                                   case v' of
                                    Just x -> search x p k
                                    Nothing -> failPCF $ "La variable '" ++ v ++ "' no está definida."

destroy :: MonadPCF m => CVal -> Kont -> m CVal
destroy v (KBinary1 op p t':k)             = search t' p (KBinary2 op v:k)
destroy v' (KBinary2 op v:k)               = destroy (destroyB op v v') k
destroy (CEKNat 0) (KCond p t _:k)         = search t p k
destroy (CEKNat _) (KCond p _ e:k)         = search e p k
destroy (CEKCl clos) (KArg p t:k)          = search t p ((KClos clos):k)
destroy v ((KClos (ClosFun p _ t)):k)      = search t (v:p) k
destroy v ((KClos fx@(ClosFix p _ _ t)):k) = search t (v:CEKCl fx:p) k
destroy v []                               = return v

destroyB (Plus _) (CEKNat n) (CEKNat n') = CEKNat $ n+n'
destroyB (Minus _) (CEKNat n) (CEKNat n') = CEKNat $ max 0 (n-n')

cvalToTerm :: CVal -> Term
cvalToTerm (CEKNat n) = Const NoPos (CNat n)
cvalToTerm (CEKCl (ClosFun p (x,xty) t)) = substN (map cvalToTerm p) (Lam NoPos x xty t)
cvalToTerm (CEKCl (ClosFix p (f,fty) (x,xty) t)) =  substN (map cvalToTerm p) (Fix NoPos f fty x xty t)

evalCEK :: MonadPCF m => Term -> m Term
evalCEK tt = do
    tt' <- search tt [] []
    return $ cvalToTerm tt'