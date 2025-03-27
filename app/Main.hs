{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-|
Module      : Main
Description : Compilador de PCF.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)

import Control.Monad
import Control.Monad.Trans
import Data.List (nub,  intersperse, isPrefixOf )
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.Environment ( getArgs )
import System.IO ( stderr, hPutStr )


import Global ( GlEnv(..) )
import Errors
import Lang ( Decl(Decl, declName, declBody), SDecl(TypeDecl), STerm, Tm(V) , Term)
import Parse ( P, parse, stm, program, declOrTm, runP )
import Elab ( elab, desugarTm, desugarTy, desugarDecl, elab_decl)
import Eval ( eval )
import PPrint ( pp , ppTy )
import MonadPCF
import TypeChecker ( tc, tcDecl )
import CC
import CP

import Debug.Trace

import System.Process
import Data.Text.Lazy (pack)
import Data.Text.Lazy.IO as TIO ( writeFile, putStrLn)

import CIR ( CanonProg(..) )

import InstSel ( codegen )

import Options.Applicative
    ( Alternative(many, (<|>)),
      (<**>),
      argument,
      flag',
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      str,
      execParser,
      helper,
      switch,
      Parser )

import CEK (evalCEK)
import Bytecompile
import LLVM.Pretty (ppllvm)
import Opt (cleanDecls, optimize, runInline)

prompt :: String
prompt = "PCF> "

data Mode = Interactive
 | Typecheck
 | Bytecompile
 | Run
 | ClosureConvert
 | CanonProgF
 | CEK

-- | Parser de banderas
parseMode :: Parser Mode
parseMode = 
    flag' Typecheck ( long "typecheck" <> short 't' <> help "Solo chequear tipos") 
  <|> flag' Bytecompile (long "bytecompile" <> short 'c' <> help "Compilar a la BVM")
  <|> flag' Run (long "run" <> short 'r' <> help "Ejecutar bytecode en la BVM") 
  <|> flag' Interactive ( long "interactive" <> short 'i'
                            <> help "Ejecutar en forma interactiva" ) 
  <|> flag' ClosureConvert (long "closure-convert" <> short 'x' <> help "Compilar a ClosureConversion") 
  <|> flag' CanonProgF (long "canon-prog" <> short 'p' <> help "Compilar a CanonProg") 
  <|> flag' CEK (long "run-cek" <> short 'k' <> help "Compilar y ejecuta en maquina abstracta CEK.") 

parseArgs :: Parser (Mode,[FilePath], Bool)
parseArgs = (,,) <$> parseMode <*> many (argument str (metavar "FILES...")) <*> switch ( long "optimize" <> short 'o' <> help "Opcional para optimizar")

go :: (Mode,[FilePath], Bool) -> IO ()
go (Typecheck, files, opt) = do
  r <- runPCF $ processFiles files (\n -> "Chequeando los tipos de: " ++ n ++ ".") processTC opt
  showRes r
go (Bytecompile, files, opt) = do 
  r <-runPCF $ processFiles files (\n -> "Compilando " ++ n ++ " a Bytecode.") processBC opt
  showRes r
go (ClosureConvert,files, opt) = do  
  r <- runPCF $ processFiles files (\n -> "Compilando " ++ n ++ " a ClosureConversion...") processCC opt
  showRes r
go (CanonProgF,files, opt) = do
  r <- runPCF $ processFiles files (\n -> "Compilando " ++ n ++ " a CanonProg...") processCP opt
  showRes r
go (CEK,files, opt) = do
  r <- runPCF $ processFiles files (\n -> "Compilando " ++ n ++ " a bytecode y evaluando en CEK...") processCEK opt
  showRes r
go (Interactive,files, opt) = do
  r <- runPCF (runInputT defaultSettings (main' files))
  showRes r
go (Run,files, opt) = do
  r <- runPCF $ runBCFiles files
  showRes r

showRes c = case c of
  Right _ -> return ()
  Left e -> print e

runBCFiles :: MonadPCF m => [FilePath] -> m ()
runBCFiles []     = return ()
runBCFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        runBCFile x
        runBCFiles xs
runBCFile :: MonadPCF m => FilePath -> m ()
runBCFile f = do
  r <- liftIO $ bcRead f
  runBC r [] []

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper) (fullDesc <> progDesc "Compilador de PCF" <> header "Compilador de PCF de la materia Compiladores 2020")


main' :: (MonadPCF m, MonadMask m) => [String] -> InputT m ()
main' args = do
        lift $ catchErrors $ compileFiles args
        s <- lift get
        when (inter s) $ liftIO $ Prelude.putStrLn
          (  "Entorno interactivo para PCF0.\n"
          ++ "Escriba :? para recibir ayuda.")
        loop  
  where loop = do
           minput <- getInputLine prompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand c
                       maybe loop (`when` loop) b


processFiles :: MonadPCF m => [String] -> (String -> String) -> (String -> String -> [Decl Lang.Term] -> m ()) -> Bool -> m ()
processFiles []     msg process opt = return ()
processFiles (x:xs) msg process opt = do
  modify (\s -> s { lfile = x, inter = False })
  processFile x msg process opt 
  processFiles xs msg process opt

processFile :: MonadPCF m => String -> (String -> String) -> (String -> String -> [Decl Lang.Term] -> m ()) -> Bool -> m ()
processFile file msg process opt = do
  -- Imprime el mensaje.
  printPCF $ msg file

  -- Obtiene el nombre puro y el nombre entero limpio.
  let (fnm, filename) = checkExtension file 
  content <- liftIO $ catch (readFile filename) (`openFileException` filename)
  
  let res = runP program content filename
  case res of  
        Right r -> do mapM_ handleDecl r
                      s <- get
                      if opt then
                        do dc <- optimize 50 (glb s)
                           printPCF (show dc)
                           process fnm filename dc
                      else process fnm filename (glb s)
        Left e -> printPCF $ "Error de parseo:" ++ show e

processCP fnm filename decls  = do
  let dcs = reverse decls
      res = runCC dcs
      res' = evalState (runCanon res) (0, "", [], [])
      llvm = codegen res'
      commandline = "clang -Wno-override-module output.ll runtime.c -lgc -o prog; ./prog"
  liftIO $ TIO.writeFile "output.ll" (ppllvm llvm)
  liftIO $ system commandline
  return ()

processCC fnm filename decls = do 
  let dcs = reverse decls
      res = runCC dcs
  liftIO $ vertListPP dcs
  liftIO $ putStr ("\n\nEl resultado se encuentra guardado en: " ++ (fnm++"_cc.out\n\n"))
  liftIO $ TIO.writeFile (fnm++"_cc.out") (pack (show res))

processCEK fnm filename decls  = do 
  l <- mapM (evalCEK.declBody) decls
  printPCF $ show (head l)
  return ()
                       
-- El handleDecl ya chequea los tipos.
-- Si no son validos, nunca llega a este print.
processTC fnm filename decls  = do
  printPCF "Tipos Correctos."
  return ()

processBC fnm filename decls = do
  let dcs = reverse decls
  bc <- bytecompileModule dcs
  liftIO $ bcWrite bc (fnm ++".byte")

-- | Utils
vertListPP :: Show a => [a] -> IO ()
vertListPP [] = return ()
vertListPP (x:xs) = do print x
                       vertListPP xs

openFileException :: IOException -> String -> IO [Char]
openFileException e filename = do let err = show (e :: IOException)
                                  hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                                  return ""

-- | Limpia el nombre de entrada y controla que la extension sea la aceptada por el compilador. Retorna el nombre limpio y el nombre
checkExtension :: String -> (String, String)
checkExtension f = do
  let f' = reverse(dropWhile isSpace (reverse f))
      (fnm, ext) = splitAt (length f - 4) f'
  if ext == ".pcf" then (fnm, f')
  else error  "El compilador sólo acepta archivos .pcf"

compileFiles ::  MonadPCF m => [String] -> m ()
compileFiles []     = return ()
compileFiles (x:xs) = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile x
        compileFiles xs

compileFile ::  MonadPCF m => String -> m ()
compileFile f = do
    printPCF ("Abriendo "++f++"...")
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStr stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err ++"\n")
                         return "")
    decls <- parseIO filename program x
    mapM_ handleDecl decls

parseIO ::  MonadPCF m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

handleDecl ::  MonadPCF m => Decl SDecl -> m ()
handleDecl (Decl p x (TypeDecl ty)) = do
  dsty <- desugarTy p ty
  addTyDcl x dsty
handleDecl d@(Decl p x t) = 
  do (tt,ty) <- desugarDecl d
     tcDecl (Decl p x tt) ty
     s <- get
     if inter s then do te <- evalCEK tt
                        addDecl (Decl p x te)
     else addDecl (Decl p x tt)

data Command = Compile CompileForm
             | Print String
             | Type String
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if ":" `isPrefixOf` x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  Prelude.putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  Prelude.putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractive x))

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   Main.Print          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadPCF m => Command  -> m Bool
handleCommand cmd = do
   s@GlEnv {..} <- get
   case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printPCF (helpTxt commands) >> return True
       Browse ->  do  printPCF (unlines [ s | s <- reverse (nub (map declName glb)) ])
                      return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e
                          CompileFile f        -> put (s {lfile=f}) >> compileFile f
                      return True
       Main.Print e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

compilePhrase ::  MonadPCF m => String -> m ()
compilePhrase x =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of 
      Left d  -> handleDecl d
      Right t -> handleTerm t

handleTerm ::  MonadPCF m => STerm -> m ()
handleTerm t = do
         dst <- desugarTm t
         let tt = elab dst
         s <- get
         ty <- tc tt (tyEnv s)
         te <- evalCEK tt
         printPCF (pp te ++ " : " ++ ppTy ty)

printPhrase   :: MonadPCF m => String -> m ()
printPhrase x =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of 
      Left d  -> printDecl d
      Right t -> printTerm t

printDecl d = do
  printPCF "SDecl:"
  printPCF $ show d
  d' <- desugarDecl d
  printPCF "Decl:"
  printPCF $ show d'

printTerm t = do 
    dex <- desugarTm t
    let ex = elab dex
    t'  <- case dex of 
           (V p f) -> maybe ex id <$> lookupDecl f
           _       -> return ex  
    printPCF "STerm:"
    printPCF (show t)
    printPCF "\nTerm:"
    printPCF (show t')
  


typeCheckPhrase :: MonadPCF m => String -> m ()
typeCheckPhrase x = do
         t <- parseIO "<interactive>" stm x
         dst <- desugarTm t
         let tt = elab dst
         s <- get
         ty <- tc tt (tyEnv s)
         printPCF (ppTy ty)