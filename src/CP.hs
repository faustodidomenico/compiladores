{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module CP where

import CIR

import CC ( Ir(..), IrDecl(IrFun, IrVal), IrDecls )

import Data.List
import Data.Either

import Debug.Trace
-- import Control.Monad.Trans.State

import Lang ( Const(CNat) , UnaryOp(Print))
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

type RegNumber = Int

type MCP = (RegNumber, Loc, [Inst], [BasicBlock])

cpAppend :: CanonProg -> CanonProg -> CanonProg
cpAppend (CanonProg cp) (CanonProg cp') = CanonProg (cp ++ cp')

runCanon :: IrDecls -> State MCP CanonProg
runCanon dc = do
    let vals = filter isVal dc
        funs = filter (not.isVal) dc
    fs <- funBlocks funs
    ns <- pcfmain vals
    bs <- getBlocks
    cleanBlocks
    return $ CanonProg $ fs ++ ns ++ [Left ("pcfmain", [], bs)]

funBlocks' (IrFun name _ args t) = do
    setLoc name
    ts <- toBasicBlock t
    closeBlock $ Return ts
    bs <- getBlocks
    cleanBlocks
    return $ Left (name, args, bs)

funBlocks [] = return []
funBlocks (f:fs) = do
    f' <- funBlocks' f 
    fs' <- funBlocks fs
    return (f' : fs')

isVal :: IrDecl -> Bool
isVal (IrVal _ _) = True
isVal _ = False

cleanBlocks = modify (\(n,l,is,_) -> (n, l, is, []))

pcfmain' [IrVal name tm] bs   = do r  <- getFreshReg ""
                                   tm' <- toBasicBlock tm
                                   newInst $ Assign r (UnOp Print tm')
                                   newInst $ Store name (V (R r))
                                   -- newInst $ Store name (V tm')
                                   return  (tm', bs ++ [Right name])
                                --    return tm'
pcfmain' ((IrVal name tm):xs) bs = do tm' <- toBasicBlock tm
                                      newInst $ Store name (V tm')
                                      pcfmain' xs (bs ++ [Right name])

pcfmain decls = do setLoc "pcfmain"
                   (val,ns) <- pcfmain' decls []
                   closeBlock (Return val)
                   return ns


getBlocks :: State MCP [BasicBlock]
getBlocks = do (_,_,_,b) <- get
               return b

getBlockList :: State MCP [Inst]
getBlockList = do (_,_,b,_) <- get
                  return b

setLoc :: Loc -> State MCP ()
setLoc nl = modify (\(n, _, is, bs) -> (n, nl, is, bs))

-- generar la nueva Loc
newBlock :: String -> State MCP Loc
newBlock s  = do
    (n, _, _, _) <- get
    modify (\(x, l, i, b) -> (x+1, l, i, b))
    return $ s ++ show n

-- limpia las instrucciones y agrega el bloque a la lista.
closeBlock :: Terminator -> State MCP ()
closeBlock t = modify (\(n, l, is, bs) -> (n, "", [], bs ++ [(l,is,t)]))

getFreshReg :: String -> State MCP Reg
getFreshReg s = do
    (n, _, _, _) <- get
    modify (\(x, l, i, b) -> (x+1, l, i, b))
    return $ Temp $ "r_" ++ s ++ show n

newInst :: Inst -> State MCP ()
newInst i = modify (\(x, l, ins, b) -> (x, l, ins ++ [i], b))

newRegAssign :: Expr -> State MCP Reg
newRegAssign exp = do
    r <- getFreshReg ""
    newInst $ Assign r exp
    return r

newRegCompute :: Ir -> State MCP (Reg,Val)
newRegCompute ir = do
    t <- toBasicBlock ir
    r <- getFreshReg ""
    newInst $ Assign r (V t)
    return (r,t)

toBasicBlock :: Ir -> State MCP Val
toBasicBlock (IrVar name)           | "__" `isPrefixOf` name = return (R (Temp name))
                                    | otherwise          = return (G name)
toBasicBlock (IrConst (CNat n)) = do
                                  r <- getFreshReg ""
                                  newInst $ Assign r (V (C n))
                                  return (R r)
toBasicBlock (IrBinaryOp bop b1 b2) = do
    r <- getFreshReg ""
    b1' <- toBasicBlock b1
    b2' <- toBasicBlock b2
    newInst $ Assign r (BinOp bop b1' b2') 
    return $ R r
toBasicBlock (CC.MkClosure n c) = do
    r <- getFreshReg ""
    c' <- mapM toBasicBlock c
    newInst $ Assign r (CIR.MkClosure n c')
    return $ R r
toBasicBlock (IrAccess c i) = do
    r <- getFreshReg ""
    c' <- toBasicBlock c
    newInst $ Assign r (Access c' i)
    return $ R r
toBasicBlock (IrCall f args) = do
    r <- getFreshReg ""
    f' <- toBasicBlock f
    args' <- mapM toBasicBlock args
    newInst $ Assign r (Call f' args')
    return $ R r
toBasicBlock (IrLet n t1 t2) = do
    t1' <- toBasicBlock t1
    newInst $ Assign (Temp n) (V t1')
    toBasicBlock t2
toBasicBlock (IrIfZ c t e) = do
    _ENTRY <- newBlock "ENTRY"
    _THEN <- newBlock "THEN"
    _ELSE <- newBlock "ELSE"
    _IFCONT <- newBlock "IFCONT"

    -- cerramos el bloque en el que veniamos trabajando.
    closeBlock $ Jump _ENTRY

    -- ENTRY BLOCK
    setLoc _ENTRY
    c' <- toBasicBlock c
    closeBlock $ CondJump (Eq c' (C 0)) _THEN _ELSE

    -- THEN BLOCK
    setLoc _THEN
    t' <- toBasicBlock t
    (_,locT,_,_) <- get
    --(_RTHEN, _) <- newRegCompute t
    closeBlock $ Jump _IFCONT

    -- ELSE BLOCK
    setLoc _ELSE
    --(_RELSE, _) <- newRegCompute e
    e' <- toBasicBlock e
    (_,locE,_,_) <- get
    --(_RTHEN, _) <- newRegCompute t
    closeBlock $ Jump _IFCONT

    setLoc _IFCONT
    _RCONT <- getFreshReg "CONT"
    newInst $ Assign _RCONT (Phi [(locT, t'), (locE, e')])
    return $ R _RCONT

-- Cada declaración puede ser o un valor o una funcion. Los valores (IrVal) se compilaran como declaraciones
-- globables que reservan una porcion de memoria (estatica), que luego sera inicializada por pcfmain, el punto de
-- entrada del programa. Las funciones IrFun ser´an canonicalizadas a una CanonFun, representadas por su nombre,
-- argumentos y lista de bloques básicos. El primer bloque basico de cada funcion marca el punto de entrada.
-- Un programa completo CanonProg consistirá entonces de una lista de declaraciones de funcion CanonFun o una
-- declaración de variables globales. Entre las CanonFun estara pcfmain, que inicializara las variables globales mediante
-- instrucciones Store.