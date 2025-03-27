{-# LANGUAGE DeriveFunctor #-}

{-|
Module      : Lang
Description : AST de términos, declaraciones y tipos
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Definiciones de distintos tipos de datos:
  - AST de términos
  - Declaraciones
  - Tipos
  - Variables

-}

module Lang where

import Common ( Pos )

-- | AST de Tipos
data Ty = 
      NatTy 
    | FunTy Ty Ty
    | NamedTy Name Ty     -- Name es para PP. Ty tendria que estar el tipo completo.
    deriving (Show,Eq)

data STy = 
      SNatTy 
    | SFunTy STy STy
    | SNamedTy Name 
    deriving (Show,Eq)

type Name = String

data Const = CNat Int
  deriving (Show, Eq)

data UnaryOp = Succ | Pred | Print -- Ver de dar soporte
  deriving (Show, Eq)

data BinaryOp = Plus Bool | Minus Bool | Prod -- Ver de dar soporte
  deriving (Show, Eq)

-- | tipo de datos de declaraciones, parametrizado por el tipo del cuerpo de la declaración
data Decl a =
    Decl { declPos :: Pos, declName :: Name,  declBody :: a }
  | Eval a
  deriving (Show,Functor, Eq)

-- | AST de los términos azucarados. 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed. 

type Binder = (Name,STy)

data SDecl = 
    Let     [Binder] STy (STm Pos Name) -- Contenido el LetF y LetComun
  | LetRec  [Binder] STy (STm Pos Name)
  | TypeDecl STy
  deriving (Show)

data STm       info var = 
     SV        info var
   | SConst    info Const
   | SApp      info (STm info var) (STm info var)
   | SUnaryOp  info UnaryOp (Maybe (STm info var))
   | SBinaryOp info BinaryOp (STm info var) (STm info var)
   | SFix      info Name STy Name STy (STm info var)
   | SIfZ      info (STm info var) (STm info var) (STm info var)
   | SLam      info [Binder] (STm info var)
   | SLet      info Binder (STm info var) (STm info var)
  --  Podemos simplificar SLet == SLetF info Name [] Ty t t'
   | SLetF     info Name [Binder] STy (STm info var) (STm info var)
   | SLetRec   info Name [Binder] STy (STm info var) (STm info var)
  deriving (Show, Functor, Eq)

-- | AST de los términos. 
--   - info es información extra que puede llevar cada nodo. 
--       Por ahora solo la usamos para guardar posiciones en el código fuente.
--   - var es el tipo de la variables. Es 'Name' para fully named y 'Var' para locally closed. 
data Tm       info var = 
     V        info var
   | Const    info Const
   | Lam      info Name Ty (Tm info var)
   | App      info (Tm info var) (Tm info var)
  --  | UnaryOp  info UnaryOp (Tm info var)      -- en un futuro, ademas de print podria estar -U.
   | BinaryOp info BinaryOp (Tm info var) (Tm info var) 
   | Fix      info Name Ty Name Ty (Tm info var)
   | IfZ      info (Tm info var) (Tm info var) (Tm info var)
   | TLet     info Name Ty (Tm info var) (Tm info var) 
  deriving (Show, Functor, Eq)

type STerm = STm Pos Name  -- ^ 'STm' tiene 'Name's como variables ligadas y libres, guarda posición.
type NTerm = Tm Pos Name   -- ^ 'Tm' tiene 'Name's como variables ligadas y libres, guarda posición.
type Term = Tm Pos Var     -- ^ 'Tm' con índices de De Bruijn como variables ligadas, different type of variables, guarda posición.

data Var = 
    Bound !Int
  | Free Name
  deriving (Show, Eq)

-- | Obtiene la info en la raíz del término.
getInfo :: Tm info var -> info
getInfo (V i _)            = i
getInfo (Const i _)        = i
getInfo (Lam i _ _ _)      = i
getInfo (App i _ _ )       = i
getInfo (BinaryOp i _ _ _) = i
getInfo (Fix i _ _ _ _ _)  = i
getInfo (IfZ i _ _ _)      = i
getInfo (TLet i _ _ _ _) = i

-- | Obtiene las variables libres de un término.
freeVars :: Tm info Var -> [Name]
freeVars (V _ (Free v))       = [v]
freeVars (V _ _)              = []
freeVars (Lam _ _ _ t)        = freeVars t
freeVars (App _ l r)          = freeVars l ++ freeVars r
freeVars (BinaryOp _ _ t t')  = freeVars t ++ freeVars t'
freeVars (Fix _ _ _ _ _ t)    = freeVars t
freeVars (IfZ _ c t e)        = freeVars c ++ freeVars t ++ freeVars e
freeVars (TLet _ _ _ t t')    = freeVars t ++ freeVars t'
freeVars (Const _ _)          = []
