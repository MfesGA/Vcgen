module Main where

import           Com
import qualified Data.Set          as Se
import           Hsmtlib
import           Hsmtlib.HighLevel as H
import           Hsmtlib.Solver as Sl
import           Hsmtlib.Parsers.Syntax  hiding(Source)
import           Parser
import           SMTLib2
import           SMTLib2.Int
import           SMTLib2.Array
import           SMTLib2.Core(not)
import           Syntax        hiding (Expr)  
import           System.IO
import           VCGen
import           Prelude hiding (not)
import           Control.Monad


prompt :: IO ()
prompt = putStr "> "

-- Inicializa o solver e faz as declarações
declareConsts :: Source -> Handle -> IO (Solver, [LogExp], Handle)
declareConsts source handle = do 
    -- Inizializa o solver
    solver <- startSolver Z3 Online AUFLIA Nothing Nothing
    -- Coloca a flag para o solver produzir modelos
    _ <- produceModels solver
    let exprs = vcgen source -- calcula as condições de verificação
    -- declare as constantes
    mapDeclConst solver (Se.toList.getVars $ exprs) tInt 
    -- declara os arays
    mapDeclConst solver (Se.toList.getExpArrays $ exprs) (tArray tInt tInt)
    return (solver, Se.toList exprs, handle)

 
-- Interface que mostra as condições e as opções
showConditions :: (Solver, [LogExp], Handle) -> IO (Solver, [LogExp], Handle)
showConditions (solver, exprs, handle) = 
    putStrLn "\n:: Verification Conditions\n" >>
    showVC exprs >>
    putStrLn "\nPara provar uma vc em particular insira o numero." >>
    putStrLn "Para provar todas insira o numero 0.\n" >>
    putStrLn "Para escolher outro ficheiro insira o numero -1.\n" >>
    putStrLn "Para sair escolha o -2.\n" >>
    return (solver, exprs, handle)


-- Imprime as condições
showVC :: [LogExp] -> IO ()
showVC  = showVC' 1

showVC' :: Int -> [LogExp] -> IO ()
showVC' _ [] = return ()
showVC' n (x:xs)= putStrLn (show n ++ " - " ++  show x) >> showVC' (n+1) xs



-- Pede o input do utilizador
getProveInput :: (Solver, [LogExp], Handle)->IO (Solver, [LogExp], Int, Handle)
getProveInput (solver, exprs, handle) = do
    prompt
    val <-fmap read getLine
    _ <- putStrLn ""
    return (solver, exprs, val, handle)


-- Realiza a operação pedida
proveHowMany :: (Solver, [LogExp], Int, Handle) -> IO ()
proveHowMany (solver, exprs, -2, handle) = exit solver >> hClose handle   
proveHowMany (solver, exprs, -1, handle) =  hClose handle >> exit solver >> main
proveHowMany (solver, exprs, 0, handle) = 
    checkAll solver exprs >> body (solver, exprs, handle) >> return ()
proveHowMany (solver, exprs, n, handle) = 
    check solver (exprs !! (n-1)) >> body (solver, exprs, handle) >> return () 


checkAll :: Solver -> [LogExp]  -> IO ()
checkAll =  checkAll' 0 

checkAll' :: Int -> Solver -> [LogExp] -> IO ()
checkAll' n solver exprs 
    | n == length exprs -1  = foo n >> return ()
    | otherwise = foo n >> checkAll' (n+1) solver exprs 
    where foo n = check solver (exprs !! n) 


check :: Solver -> LogExp -> IO ()
check solver expr = do
    let sexpr = createSexpr expr
    _ <- push solver 1
    _ <- assert solver $ not sexpr
    val <- checkSat solver
    case val of
        CCS Unsat -> putStrLn $ "The condition: " ++ show expr ++ " is valid."
        CCS _ ->  putStrLn ("The condition: " ++ show expr  ++ " is not valid." ) 
                >> askForModel solver 
        (ComError err) -> print $ "Error: " ++ err
        _ -> putStrLn "Error in Main.hs, function check!"
    _ <- pop solver 1
    return ()

askForModel :: Solver -> IO ()
askForModel s = putStrLn "Deseja ver um contra exemplo? [y/n]" >>
               getLine >>= askForModel' s

askForModel' :: Solver -> String -> IO ()
askForModel' _ "n" = return ()
askForModel' s "y" = putStrLn "Insira o nome da variavel." >>
                     putStrLn "Insira a pos do array se for o caso." >>
                     putStrLn "e.g.: a | x 2\n" >> 
                     getLine >>= showValue s.words
askForModel' _ _ = print "Opção errada!"


showValue :: Solver -> [String] -> IO ()
showValue solver (x:[]) = getValue solver [ct x] >>= print
showValue solver (x:y:[]) =
    getValue solver [ct ("(select " ++ x ++ " " ++ y ++")")] >>= print
showValue _ _ = print "Opção errada!"






-- Main
main :: IO ()
main = putStrLn "Insira o caminho para o ficheiro." >>
       prompt >> getLine  >>= runProgram


runProgram :: String -> IO ()
runProgram fpath = do
    fHandle <- openFile fpath ReadMode 
    file <- hGetContents fHandle >>= parseFile 
    case file of
        Left err -> print err >> hClose fHandle
        Right source -> interface source fHandle
    putStr "\n\n"

interface :: Source -> Handle -> IO ()
interface src handle = 
    declareConsts src handle >>= -- declara as condições e calcula as condições
    body


body :: (Solver, [LogExp], Handle) -> IO ()
body =  proveHowMany -- Prova as pedidas
    <=< getProveInput -- recebe o valor do utilizador
    <=< showConditions -- Mostra as condições e prompt