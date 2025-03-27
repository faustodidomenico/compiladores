{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Byecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental
Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM 
para ejecutar bytecode.
-}
module Bytecompile
  (Bytecode, 
  bytecompileModule,
  runBC,
  bcWrite,
  bcRead,
  run)
 where

import Lang 
import Subst
import MonadPCF

import qualified Data.ByteString.Lazy as BS
import Data.Binary ( Word32, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le, isEmpty )
import Common

type Opcode = Int
type Bytecode = [Opcode]

newtype Bytecode32 = BC { un32 :: [Word32] }

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go 
    where go =  
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord32le
                      BC xs <- go
                      return $ BC (x:xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:
 
   f (CALL : cs) = ...
 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.
 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern PLUS     = 6
pattern MINUS    = 7
pattern IFZ      = 8
pattern FIX      = 9
pattern STOP     = 10
pattern JUMP     = 11
pattern SHIFT    = 12
pattern DROP     = 13
pattern PRINT    = 14
pattern TAILCALL = 15

bc :: MonadPCF m => Term -> m Bytecode
bc (Const _ (CNat n))  = return [CONST, n]
bc (BinaryOp _ (Minus _) t t')  = do ct <- bc t
                                     ct' <- bc t'
                                     return (ct++ct'++[MINUS])
bc (BinaryOp _ (Plus _) t t')  = do ct <- bc t
                                    ct' <- bc t'
                                    return (ct++ct'++[PLUS])
bc (IfZ _ c t e)       = do cc <- bc c 
                            ct <- btc t
                            ce <- btc e
                            let lct = 2 + length ct 
                                lce = length ce
                            return $ cc ++ [IFZ,lct] ++ ct ++ [JUMP,lce] ++ ce
bc (App _ t u)         = do ct <- bc t
                            cu <- bc u
                            return (ct++cu++[CALL])
bc (Lam _ _ _ t)      = do ct <- btc t
                           return $ [FUNCTION,length ct]++ct
bc (Fix _ _ _ _ _ t)  = do ct <- btc t
                           return $ [FUNCTION,length ct] ++ ct ++ [FIX]
bc (TLet _ _ _ t1 t2) = do ct1 <- bc t1
                           ct2 <- bc t2
                           return $ ct1++SHIFT:ct2++[DROP]
bc (V _ (Bound x))     = return $ [ACCESS,x]
bc (V _ (Free _))      = error "Error: El término no puede tener variables libres."  



btc :: MonadPCF m => Term -> m Bytecode
btc (App _ t u) = do t' <- bc t
                     u' <- bc u
                     return $ t' ++ u' ++ [TAILCALL]
btc (TLet _ _ _ m n) = do m' <- bc m
                          n' <- btc n
                          return $ m' ++ [SHIFT] ++ n'
btc t = do t' <- bc t
           return $ t' ++ [RETURN]

type Module = [Decl Term]

bytecompileModule :: MonadPCF m => Module -> m Bytecode
bytecompileModule ds = do
  ds' <- transformDecls ds
  printPCF $ show ds'
  bc' <- bc ds'
  let res = bc' ++ [PRINT,STOP]
  printPCF $ show res
  return res
    
transformDecls :: MonadPCF m => Module -> m Term
transformDecls [] = error "Programa nulo."
transformDecls [Decl p n t] = do
  ty <- lookupTy n 
  case ty of
    Just NatTy -> return t
    Just x -> error $ "Se esperaba tipo Nat en la última declaración, y se obtuvo: " ++ show x
    _ -> error "Declaración inválida."
transformDecls ((Decl p n t):xs) = do 
  ty <- lookupTy n 
  case ty of
    Just ty' -> do
      xs' <- transformDecls xs
      return $ TLet p n ty' t (close n xs')
    Nothing  -> error "Declaración inválida."
  

-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename


runBC :: MonadPCF m => Bytecode -> Env -> Stack -> m ()
runBC (CONST:n:c) e s  = runBC c e ((I n):s)
runBC (RETURN:_) _ (v:(RA e c):s)  = runBC c e (v:s)
runBC (ACCESS:i:c) e s = runBC c e (e!!i:s)
runBC (FUNCTION:lc:c) e s = runBC (drop lc c) e ((Fun e c):s)
runBC (TAILCALL:_) _ (v:(Fun eg cg):(RA e' c'):s) = runBC cg (v:eg) ((RA e' c'):s)
runBC (TAILCALL:_) _ _    = error "Error  TAILCALL"
runBC (CALL:c)  e (v:(Fun ef cf):s) = runBC cf (v:ef) ((RA e c):s)
runBC (CALL:_) _ _    = error "Error CALL"
runBC (PLUS:c) e (I n:I n':s) = runBC c e (I (n'+n):s)
runBC (PLUS:_) _ _       = error "Error SUCC"
runBC (MINUS:c) e (I n:I n':s) = runBC c e (I (max 0 (n'-n)):s)
runBC (MINUS:_) _ _       = error "Error PRED"
runBC (SHIFT:c) e (v:s) = runBC c (v:e) s 
runBC (DROP:c) (_:e) s  = runBC c e s
runBC (FIX:xs) e ((Fun _ bc):s) = runBC xs e ((Fun ((Fun efix bc):e) bc):s)
                                   where efix = (Fun efix bc):e
runBC (IFZ:_:c) e (I 0:s)    = runBC c e s
runBC (IFZ:lct:c) e (I _:s)    = runBC (drop lct c) e s
runBC (IFZ:_) _ _    = error "Error IFZ"
runBC (JUMP:n:c) e v = runBC (drop n c) e v
runBC (PRINT:c) e s@(I n:_) = do
  printPCF $ show n
  runBC c e s
runBC (STOP:_) _ _ = return ()


run :: MonadPCF m => Bytecode -> m ()
run bc = runBC bc [] []

type Env = [Val]
type Stack = [Val]
data Val = I Int | Fun Env Bytecode | RA Env Bytecode
 deriving Show