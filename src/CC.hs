{-# LANGUAGE FlexibleContexts #-}
module CC where
import Lang ( Var(Free), Term, Tm(TLet, V, Const, App, Lam, BinaryOp, Fix, IfZ), Decl(Decl), BinaryOp, Const, Name, freeVars )
import Control.Monad.State ( modify, MonadState(get), StateT(runStateT) )
import Control.Monad.Writer ( runWriter, MonadWriter(tell), Writer )
import Data.List 
import Subst ( open, openN )
import Debug.Trace
import Common (rmdups)

type MonadCC = (StateT Int (Writer [IrDecl]))

data Ir = IrVar Name
 | IrCall Ir [Ir]
 | IrConst Const
 | IrBinaryOp BinaryOp Ir Ir
 | IrLet Name Ir Ir
 | IrIfZ Ir Ir Ir
 | MkClosure Name [Ir]
 | IrAccess Ir Int
 deriving Show

data IrDecl = IrFun {irDeclName :: Name, irDeclArity :: Int, irDeclArgNames :: [String], irDeclBody :: Ir}
            | IrVal {irDeclName :: Name, irDeclDef :: Ir} 
            deriving Show

type IrDecls = [IrDecl]


-- Funciones de utilidad para manejo de la monada de estado.
getName :: String -> StateT Int (Writer [IrDecl]) String
getName s = do
    v <- get
    modify (+1)
    return $ "__"++s++show v

getFunName = getName ""
getVarName = getName
getClosName = getName "clos"


-- | Define un let por cada variable libre.
makeBlock' :: Name -> [Name] -> Ir -> Int -> Ir
makeBlock' _ [] t _         = t 
makeBlock' name (var:vars) t n = IrLet var (IrAccess (IrVar name) n) (makeBlock' name vars t (n+1))

makeBlock :: Name -> [Name] -> Ir -> Ir
makeBlock n fv t = makeBlock' n fv t 1

makeBlockR :: Name -> [Name] -> Ir -> Ir
makeBlockR name (f:vars) t = IrLet f (IrVar name) (makeBlock name vars t)


-- | Obtiene la aridad de las funciones. Como todas son currificadas (se evalúan de a un argumento) la aridad SIEMPRE es 2.
funAr :: Int
funAr = 2

-- | Obtiene las variables libres de un término. Exceptuando las que son TOP-LEVEL.
fVars' :: Term -> [Name]
fVars' ns = rmdups $ filter (isPrefixOf "__") (freeVars ns)

-- | Convierte la lista de nombres 
names2Ir :: [Name] -> [Ir]
names2Ir = map IrVar

closureConvert ::  Term ->  MonadCC Ir
closureConvert  (V        _ (Free n))      = return $ IrVar n
closureConvert  (Const    _ n)             = return $ IrConst n

closureConvert  (App      _ f x)          = do
    ff <- closureConvert f
    xx <- closureConvert x
    n <- getName "a"
    return $ IrLet n ff (IrCall (IrAccess (IrVar n) 0) [IrVar n,xx]) 
closureConvert  (Lam _ n _ t)        = do
    varName  <- getVarName n
    funName  <- getFunName 
    closName <- getClosName
    let --t' = open varName t
        fv = fVars' t
    t' <- closureConvert (open varName t)
    tell [IrFun { irDeclName = funName,
                  irDeclArity = funAr, 
                  irDeclArgNames = [closName, varName],
                  irDeclBody = makeBlock closName fv t'
                }]  
    return $ MkClosure funName (names2Ir fv)

closureConvert  (BinaryOp _ bop t t')      = do 
    tt <- closureConvert t
    tt'<- closureConvert t'
    return $ IrBinaryOp bop tt tt'

closureConvert  (Fix      _ f _ x _ t) = do
    varName  <- getVarName x
    funName  <- getFunName
    closName <- getClosName
    rfunName <- getVarName f
    let fv = fVars' t
    tt <- closureConvert (openN [rfunName,varName] t)
    tell [IrFun { irDeclName = funName,
                  irDeclArity = funAr, 
                  irDeclArgNames = [closName, varName],
                  irDeclBody = makeBlockR closName (rfunName:fv) tt
                }] 
    return $ MkClosure funName (names2Ir fv)

closureConvert  (IfZ      _ c t e)         = do
    cc <- closureConvert c
    tt <- closureConvert t
    ee <- closureConvert e
    return $ IrIfZ cc tt ee

closureConvert  (TLet     _ x _ t t')     = do
    cc <- closureConvert t
    nn <- getName x
    tt <- closureConvert (open nn t')
    return $ IrLet nn cc tt


runCC' :: [Decl Term] -> MonadCC ()
runCC' [] = return ()
runCC' ((Decl _ n b):xs) = do t <- closureConvert b
                              tell [IrVal
                                    {
                                      irDeclName = n, 
                                      irDeclDef = t
                                    }]
                              runCC' xs

runCC :: [Decl Term] -> IrDecls
runCC m = snd $ runWriter (runStateT (runCC' m) 0)