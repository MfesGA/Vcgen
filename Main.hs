module Main where

import           Com
import qualified Data.Set          as Se
import           Hsmtlib
import           Hsmtlib.HighLevel as H
import           Hsmtlib.Solver as Sl
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



check :: Solver -> (String, Expr) -> IO SatResult
check solver (psexpr,sexpr) = do
    push solver 1
    assert solver $ not  sexpr
    val <- checkSat solver
    case val of
        Unsat -> print $ "The condition: " ++ psexpr ++ " is valid."
        SUError err -> print $ "Error: " ++ err
        _ ->  putStrLn ("The condition: " ++ psexpr ++ " is not valid." ) >> 
                askForModel solver
    pop solver 1 
    return val
 


askForModel :: Solver -> IO ()
askForModel s = putStrLn "Deseja ver um contra exemplo? [y/n]" >>
               getChar >>= askForModel' s

askForModel' :: Solver -> Char -> IO ()
askForModel' _ 'n' = return ()
askForModel' s 'y' = putStrLn "Insira o nome da variavel." >>
                   putStrLn "Insira a pos do array se for o caso." >>
                   putStrLn "e.g.: a | x 2\n" >> 
                   getLine >> getLine >>= (showValue s).words
askForModel' _ _ = print "Opção errada!"


showValue :: Solver -> [String] -> IO ()
showValue solver (x:[]) = getValue solver [constant x] >>= print
showValue solver (x:y:[]) =
    getValue solver [constant ("(select " ++ x ++ " " ++ y ++")")] >>= print
showValue _ _ = print "Opção errada!"


isValid :: [IO SatResult] -> IO ()
isValid [] = print "The program is valid"
isValid (x:xs) = do
        val <- x
        case val of 
            Unsat -> isValid xs
            _ -> print "The program is not valid"

showCV :: [(String,Expr)] -> IO ()
showCV = showCV' 1

showCV' :: Int -> [(String,Expr)] -> IO ()
showCV' _ [] = return ()
showCV' n ((x,_):xs) = putStr (show n ++ " -> " )>> putStrLn x >> showCV' (n+1) xs 

showAsserts :: Solver -> [(String,Expr)] -> IO ()
showAsserts solver zips =
    putStrLn "\n:: Verification Conditions\n" >>
    showCV zips >>
    putStrLn "\nPara provar uma vc em particular insira o numero." >>
    putStrLn "Para provar todas insira o numero 0.\n" >>
    fmap read getLine >>= checkWhat solver zips


checkWhat :: Solver -> [(String, Expr)] -> Int -> IO ()
checkWhat solver zips 0 = checkAll solver zips
checkWhat solver zips n = checkN solver zips (n-1)

checkAll :: Solver -> [(String, Expr)] -> IO ()
checkAll solver zipAsserts = isValid $ map (check solver) zipAsserts


checkN :: Solver -> [(String, Expr)] -> Int -> IO ()
checkN solver zips n = void $ check solver (zips !! n)

runSolver :: Source -> IO String
runSolver source = do
    let exprs = vcgen source
    let asserts = fmap createSexpr (Se.toList exprs) 
    let zipAsserts = zip (fmap  show (Se.toList exprs)) asserts
    solver <- startSolver Z3 Online QF_AUFLIA Nothing Nothing
    produceModels solver
    -- declare constants
    mapDeclConst solver (Se.toList.getVars $ exprs) tInt 
    -- declare arrays
    mapDeclConst solver (Se.toList.getArrays $ source) (tArray tInt tInt)
    showAsserts solver zipAsserts
    --isValid $ map (check solver) zipAsserts
    -- assert Side Conditions
    exit solver





-- Main
main :: IO ()
main = forever run

prompt :: IO ()
prompt = putStr "> "

run :: IO ()
run = putStrLn "Insira o caminho para o ficheiro." >>
       prompt >> getLine  >>= runProgram


runProgram :: String -> IO ()
runProgram path = do
    fHandle <- openFile path ReadMode 
    file <- hGetContents fHandle >>= parseFile 
    case file of
        Left err -> print err >> hClose fHandle
        Right source -> runSolver source >> hClose fHandle
    putStr "\n\n"
