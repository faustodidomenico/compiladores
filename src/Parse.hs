{-|
Module      : Parse
Description : Define un parser de términos PCF0 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (stm, Parse.parse, decl, runP, P, program, declOrTm) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec hiding (runP)
import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language ( GenLanguageDef(..), emptyDef )

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "fun", "fix", "then", "else",
                          "succ", "pred", "ifz", "Nat", "in", "rec", "type"],
         reservedOpNames = ["->",":","=","+","-"]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer
natural = Tok.natural lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyvar :: P Name
tyvar = Tok.lexeme lexer $ do
 c  <- upper
 cs <- option "" identifier
 return (c:cs)

tyatom :: P STy
tyatom = (reserved "Nat" >> return SNatTy)
         <|> parens typeP
         <|> (do SNamedTy <$> tyvar)

typeP :: P STy
typeP = try (do
          x <- tyatom
          reservedOp "->"
          SFunTy x <$> typeP)
      <|> tyatom

const :: P Const
const = CNat <$> num

unaryOpName :: P UnaryOp
unaryOpName =
      (reserved "succ" >> return Succ)
  <|> (reserved "pred" >> return Pred)

sunaryOp :: P STerm
sunaryOp = do
  i <- getPos
  o <- unaryOpName
  try (do SUnaryOp i o . Just <$> satom) <|> return (SUnaryOp i o Nothing)

-- unaryOp :: P STerm
-- unaryOp = do
--   i <- getPos
--   o <- unaryOpName
--   a <- satom
--   return (SUnaryOp i o (Just a))

--binaryOpName :: P BinaryOp
binaryOpName =
  (reservedOp "+" >> return (SBinaryOp NoPos (Plus False)))
  <|> (reservedOp "-" >> return (SBinaryOp NoPos (Minus False)))

sbinaryOp :: P STerm
sbinaryOp = chainl1 (try sapp <|> satom) binaryOpName

satom :: P STerm
satom = (flip SConst <$> const <*> getPos)
       <|> flip SV <$> var <*> getPos
       <|> sunaryOp
       <|> parens stm

slam :: P STerm
slam = do i <- getPos
          reserved "fun"
          b <- manyBinders
          reservedOp "->"
          SLam i b <$> stm

-- Nota el parser app también parsea un solo atom.
sapp :: P STerm
sapp = do i <- getPos
          f <- satom
          args <- many1 satom
          return (foldl (SApp i) f args)

sifz :: P STerm
sifz = do i <- getPos
          reserved "ifz"
          c <- stm
          reserved "then"
          t <- stm
          reserved "else"
          SIfZ i c t <$> stm

binding :: P (Name, STy)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)


sfix :: P STerm
sfix = do i <- getPos
          reserved "fix"
          (f, fty) <- parens binding
          (x, xty) <- parens binding
          reservedOp "->"
          SFix i f fty x xty <$> stm

-- Parser de definiciones locales.
slet :: P STerm
slet = do
  pos <- getPos
  reserved "let"
  (v,ty) <- binding
  reservedOp "="
  t <- stm
  reserved "in"
  SLet pos (v,ty) t <$> stm

sletF :: P STerm
sletF = do
  pos <- getPos
  reserved "let"
  f <- var
  b <- manyBinders
  reservedOp ":"
  ty <- typeP
  reservedOp "="
  t <- stm
  reserved "in"
  SLetF pos f b ty t <$> stm

sletRec :: P STerm
sletRec = do
  pos <- getPos
  reserved "let"
  reserved "rec"
  f <- var
  b <- manyBinders
  reservedOp ":"
  ty <- typeP
  reservedOp "="
  t <- stm
  reserved "in"
  SLetRec pos f b ty t <$> stm

-- | Parser de términos
stm :: P STerm
stm =
      slam            <|>
      sifz            <|>
      sunaryOp        <|>
      try sbinaryOp   <|>
      try sapp        <|>
      try sfix        <|>
      (try slet       <|>
      try sletF       <|>
      try sletRec  )

-- | Parser de declaraciones
decl :: P (Decl SDecl)
decl =  try declLet <|> declType

declLet = do
     i <- getPos
     reserved "let"
     b <- optionalS "rec"
     v <- var
     bs <- many0Binders
     reservedOp ":"
     ty <- typeP
     reservedOp "="
     t <- stm
     if b then return (Decl i v (LetRec bs ty t)) else return (Decl i v (Let bs ty t))

declType = do
  i <- getPos
  reserved "type"
  v <- var
  reservedOp "="
  Decl i v . TypeDecl <$> typeP

optionalS s = try (do reserved s
                      return True) <|> return False


-- | Parser de programas (listas de declaraciones) 
-- program :: P [Decl NTerm]
program :: P [Decl SDecl]
program = many decl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (Decl SDecl) STerm)
declOrTm =  try (Right <$> stm) <|> (Left <$> decl)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

-- Para debugging en uso interactivo (ghci)
parse :: String -> STerm
parse s = case runP stm s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)

-- Utilidades de Syntactic Sugar.
manyBinders :: P [(Name, STy)]
manyBinders = do
  bs <- many1 (parens binders)
  return (concat bs)

many0Binders :: P [(Name, STy)]
many0Binders = do
  bs <- many (parens binders)

  return (concat bs)

binders :: P [(Name, STy)]
binders = do v <- many1 var
             reservedOp ":"
             ty <- typeP
             return (map (\x -> (x,ty)) v)