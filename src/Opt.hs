{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Opt (cleanDecls, optimize, runInline) where

import Control.Monad.State
import Data.List
import Data.Maybe
import Lang
import Subst (subst, open, close, openN)
import qualified Common
import Debug.Trace
import Common (rmdups)
import MonadPCF (MonadPCF, printPCF)

-- Simplifica sumas y restas de expresiones constantes, elimina ramas del if, y reemplaza una variable libre si es una expresiòn constante.
cf :: Term -> State OptState Term 
cf m@(V _ (Free v)) = do
  (_,ds) <- get
  case search v ds of
    Just (Decl _ n (Const i (CNat k))) -> return $ Const i (CNat k)
    _ -> return m
cf (Lam i n ty t) = do
  t' <- cf t
  return $ Lam i n ty t
cf (App i t1 t2) = do
  t1'<- cf t1
  t2'<- cf t2
  return $ App i t1' t2'
cf (BinaryOp i (Plus _) (Const _ (CNat n1)) (Const _ (CNat n2))) = return $ Const i (CNat (n1 + n2))
cf (BinaryOp i (Minus _) (Const _ (CNat n1)) (Const _ (CNat n2))) = return $ Const i (CNat (max 0 (n1 - n2)))
cf (BinaryOp i op t1 t2) = do
  t1' <- cf t1
  t2' <- cf t2
  return $ BinaryOp i op t1' t2'
  
cf (Fix i n1 ty1 n2 ty2 t) = do
  t' <- cf t
  return $ Fix i n1 ty1 n2 ty2 t'
cf (IfZ i c t e) = do
  c' <- cf c
  case c' of
   (Const _ (CNat 0)) -> cf t
   (Const _ (CNat _)) -> cf e
   c' -> do
     t' <- cf t
     e' <- cf e
     return $ IfZ i c' t' e'
cf (TLet i n ty t1 t2) = do
  t1' <- cf t1
  t2' <- cf t2
  return $ TLet i n ty t1' t2'
cf t = return t

-- Elimina las declaraciones que no son referenciadas.
cleanDecls :: Decl Term -> [Decl Term] -> [Decl Term]
cleanDecls d ds =
  let fv = freeVars (declBody d)
   in cleanDecls' ds fv [d]

cleanDecls' :: [Decl Term] -> [String] -> [Decl Term] -> [Decl Term]
cleanDecls' _ [] res = res
cleanDecls' ds (x : xs) res =
  case search x ds of
    Nothing -> cleanDecls' ds xs res
    Just d ->
      if d `notElem` res
        then cleanDecls' ds (rmdups $ xs ++ freeVars (declBody d)) (res ++ [d])
        else cleanDecls' ds xs res

search :: Name -> [Decl a] -> Maybe (Decl a)
search x ds =
  case filter (\d -> declName d == x) ds of
    [] -> Nothing
    res -> Just (head res)


type OptState = (Int, [Decl Term])

iast :: Int -> [Term] -> Term -> State OptState Term
iast k s e =
  case e of
    BinaryOp i op t1 t2 -> do
      t1' <- iast k s t1
      t2' <- iast k s t2
      return $ BinaryOp i op t1' t2'
    IfZ i c t e -> do
      c' <- iast k s c
      t' <- iast k s t
      e' <- iast k s e
      return $ IfZ i c' t' e'
    Lam i n ty t -> do
      t' <- iast k s (open n t)
      return $ Lam i n ty (close n t')
    TLet i n ty t1 t2 -> do
      t1' <- iast k s t1
      t2' <- iast k s t2
      return $ TLet i n ty t1' (close n t2')
    App i f args -> do
      args' <- iast k s args
      f' <- iast k s f
      iapp k s i f' args'
    e -> return e

iapp :: Int -> [Term] -> Common.Pos -> Term -> Term -> State OptState Term
iapp k s i f@(V _ (Free v)) e = do
  (_,ds) <- get
  case search v ds of
    Just (Decl _ n b) -> if isRecursive b
      then do return $ App i f e 
      else iapp k s i b e
    Nothing -> error "No es una funcion."
iapp k s i f e =
  case f of 
    App {} -> return $ App i f e
    _ -> if shouldInline k s f (fWidth f + 1)
    then ilet k s i f e
    else return $ App i f e

ilabel k s i f@(Fix _ fn fty x xty t) e = do
  return f

ilet :: Int -> [Term] -> Common.Pos -> Term -> Term -> State OptState Term
ilet k s i (TLet _ n ty f e) e' = do 
  e'' <- ilet k s i e (close n e')
  return $ TLet i n ty f e''

ilet k s i f e = do
  let fn = getArg f
  (fb,ty) <- getBody f
  f' <- iast (k-1) (f : s) (open fn fb)
  case e of
    Const {} -> return $ subst e (close fn f')
    V {}     -> return $ subst e (close fn f')
    _ -> do 
      n <- freshName 
      return $ TLet i n ty e (open n (close fn f'))

-- Funciones de utilidad.
freshName :: State OptState String
freshName = do
  (i, ds) <- get
  modify (const (i + 1, ds))
  return $ "__x" ++ show i

-- Cantidad de argumentos que tiene una funcion.
fWidth :: Term -> Int
fWidth (Lam _ _ _ t) = 1 + fWidth' t
fWidth (Fix _ _ ty _ _ _) = fWidth'' ty
fWidth t = 0

fWidth' :: Term -> Int
fWidth' (Lam _ _ _ t) = 1 + fWidth' t
fWidth' _ = 0

fWidth'' :: Ty -> Int
fWidth'' (FunTy _ t) = 1 + fWidth'' t
fWidth'' (NamedTy _ t) = fWidth'' t
fWidth'' _ = 0

-- Profundidad de un termino, utilizado para calcular el tamaño del cuerpo de una funcion.
fDepth :: Term -> Int
fDepth (Lam _ _ _ t) = 1 + fDepth t 
fDepth (Fix _ _ _ _ _ t) = fDepth t
fDepth (BinaryOp i op t1 t2) = 1 + max (fDepth t1) (fDepth t2)
fDepth (IfZ _ c t e) = 1 + max (fDepth c) (max (fDepth t) (fDepth e))       
fDepth (TLet _ _ _ t1 t2) = 1 + max (fDepth t1) (fDepth t1)
fDepth (App _ f t) = 1 + max (fDepth f) (fDepth t)
fDepth _ = 0

isRecursive :: Term -> Bool
isRecursive Fix {} = True
isRecursive _ = False

getArg :: Term -> String
getArg (Lam _ n _ _) = n
getArg (Fix _ _ _ n _ _) =  n
getArg t = error ("Error in getArg: " ++ show t)

getBody :: Term -> State OptState (Term, Ty)
getBody (Lam _ _ ty t) = return (t,ty)
getBody (Fix _ _ ty _ _ t) = return (t,ty)
getBody t = return (t, NatTy)

shouldInline :: Int -> [Term] -> Term -> Int -> Bool
shouldInline k s f csize = fDepth f < k * csize

getDecls :: State OptState [Decl Term]
getDecls = do (_, ds) <- get
              return ds

-- Funciones para correr las optimizaciones sobre las declaraciones.
optimize :: MonadPCF m => Int -> [Decl Term] -> m [Decl Term]
optimize n ds = optimize' n ds (runOptimizations ds) 

optimize' :: MonadPCF m => Int -> [Decl Term] -> [Decl Term] -> m [Decl Term]
optimize' 0 anterior actual = do 
    traceM $ "Límite de iteraciones alcanzado: " ++ show actual
    return actual
optimize' n anterior actual
  | anterior == actual = do 
    traceM ("No hubo cambios entre iteraciones. Cantidad iteraciones totales: " ++ show (50 - n))
    return actual
  | otherwise = do
    optimize' (n-1) actual (runOptimizations actual)

runOptimizations :: [Decl Term] -> [Decl Term]
runOptimizations ds = 
  let cf = runConstFolding ds
      dc = runDeadCode cf
  in runInline dc

runConstFolding :: [Decl Term] -> [Decl Term]
runConstFolding ds = map  (\d -> 
  let (d',_) = runState (cf (declBody d)) (0, ds)
  in d {declBody = d'}) ds

runDeadCode :: [Decl Term] -> [Decl Term]
runDeadCode decls@(d:ds) = cleanDecls d decls

runInline :: [Decl Term] -> [Decl Term]
runInline ds = map (\d -> 
  let (d',_) = runState (iast 50 [] (declBody d)) (0, ds)
  in d {declBody = d'}) ds
