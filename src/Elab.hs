{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@) 
-}

module Elab ( elab, elab_decl, desugarTm, desugarTy, desugarBs, desugarDecl) where

import Lang
import Subst
import MonadPCF

import Common (Pos)

desugarDecl :: MonadPCF m => Decl SDecl -> m (Term, Ty)    
desugarDecl (Decl p _ (Let bs ty t)) = do
    t' <- desugarTm t
    bs' <- mapM (desugarBs p) bs
    let dst = dsLam p bs' t'
        tt = elab dst
    ty' <- desugarTy p (makeFun (map snd bs) ty)
    return (tt, ty')
desugarDecl (Decl p x (LetRec bs ty t)) = 
  do mf1 <- desugarTy p (makeFun (map snd bs) ty)
     bs' <- mapM (desugarBs p) bs
     dt  <- desugarTm t
     let dst = Fix p x mf1 (fst $ head bs) (snd $ head bs') (dsLam p (tail bs') dt)
         tt = elab dst
     return (tt, mf1)
     
-- Maneja errores.
desugarTy :: MonadPCF m => Pos
                      -> STy
                      -> m Ty
desugarTy _ SNatTy = return NatTy
desugarTy p (SFunTy t1 t2) =  do dst1 <- desugarTy p t1
                                 dst2 <- desugarTy p t2
                                 return $ FunTy dst1 dst2
desugarTy p (SNamedTy n) = do t <- lookupTyDcl n
                              case t of
                                Just t' -> return (NamedTy n t')
                                Nothing -> failPosPCF p $ "Error: El tipo con nombre " ++ n ++ " no está declarado."

desugarBs :: MonadPCF m => Pos -> Binder -> m (Name,Ty)
desugarBs p (n,sty) = do
    dsty <- desugarTy p sty
    return (n,dsty) 

desugarTm :: MonadPCF m => STerm
                        -> m NTerm
desugarTm (SV p v) = return (V p v)
desugarTm (SConst p c) = return (Const p c)

desugarTm (SApp p (SUnaryOp _ o Nothing) a) = do 
    a' <- desugarTm a
    return (BinaryOp p (mapUnary o) a' (Const p (CNat 1)))
desugarTm (SApp p h a) = do 
    h' <- desugarTm h
    a' <- desugarTm a
    return (App p h' a') 
desugarTm (SUnaryOp p o (Just t)) = do 
    t' <- desugarTm t
    return (BinaryOp p (mapUnary o) t' (Const p (CNat 1)))
desugarTm (SUnaryOp p o Nothing) = do 
    return (Lam p "x" NatTy (BinaryOp p (mapUnary o) (V p "x") (Const p (CNat 1))))
desugarTm (SBinaryOp p o t1 t2) = do 
    t1' <- desugarTm t1
    t2' <- desugarTm t2
    return (BinaryOp p o t1' t2')
desugarTm (SFix p f fty x xty t) = do
    dfty <- desugarTy p fty
    dxty <- desugarTy p xty
    t' <- desugarTm t
    return (Fix p f dfty x dxty t')
desugarTm (SIfZ p c t e) = do 
    c' <- desugarTm c
    t' <- desugarTm t
    e' <- desugarTm e
    return (IfZ p c' t' e')  
desugarTm (SLam p bs t) = do 
    bs' <- mapM (desugarBs p) bs
    t' <- desugarTm t
    return (dsLam p bs' t')
desugarTm (SLet p (v, ty) t t') = do
    ty' <- desugarTy p ty
    dt  <- desugarTm t
    dt' <- desugarTm t'
    return $ TLet p v ty' dt dt'
    -- return $ App p (Lam p v ty' dt') dt
desugarTm (SLetF p f bs ty t t') = dsLetF p f bs ty t t'
desugarTm (SLetRec p f bs ty t t') = dsLetRec p f bs ty t t'

dsLam _ [] t = t
dsLam p [(v,ty)] t = Lam p v ty t
dsLam p ((v,ty):bs) t = Lam p v ty (dsLam p bs t)

mapUnary Succ = Plus True
mapUnary Pred = Minus True

-- let f (g : Nat -> Nat) : Nat = t in f succ

-- Binder = (Name,STy)
dsLetF p f bs fty t t' = do
    let ts = makeFun (map snd bs) fty
        bd = (f,ts)
    desugarTm (SLet p bd (SLam p bs t) t')

dsLetRec p f [(v,ty)] fty t t' = desugarTm $ SLet p (f, SFunTy ty fty) (SFix p f (SFunTy ty fty) v ty t) t'
dsLetRec p f (b:bs) fty t t' = do
    let tys  = makeFun (map snd bs) fty
        funs = SLam p bs t
    dsLetRec p f [b] tys funs t'

makeFun :: [STy] -> STy -> STy
makeFun [] ty = ty
makeFun [t] ty = SFunTy t ty
makeFun (t:ts) ty = SFunTy t (makeFun ts ty)


-- | 'elab' transforma variables ligadas en índices de 'de Bruijn'
-- en un término dado. 
elab :: NTerm -> Term
elab (V p v)               = V p (Free v)
elab (Const p c)           = Const p c
elab (Lam p v ty t)        = Lam p v ty (close v (elab t))
elab (App p h a)           = App p (elab h) (elab a)
elab (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab t))
elab (IfZ p c t e)         = IfZ p (elab c) (elab t) (elab e)
elab (BinaryOp i o t t')       = BinaryOp i o (elab t) (elab t')
elab (TLet p n ty t1 t2)        = TLet p n ty (elab t1) (close n (elab t2))

elab_decl :: Decl NTerm -> Decl Term
elab_decl = fmap elab