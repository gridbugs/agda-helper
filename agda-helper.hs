import Control.Monad
import Control.Exception
import System.Environment
import Data.Char
import Agda.Interaction.GhciTop
import Agda.Interaction.BasicOps



loadAgdaFile :: FilePath -> IO ()
loadAgdaFile file = ioTCM file 
                          (Just "/tmp/blah") 
                          (cmd_load file [".", "/usr/share/agda-stdlib"] )


getExprType :: FilePath -> String -> IO()
getExprType file expr = ioTCM file
                        Nothing
                        (cmd_infer_toplevel Normalised expr)
                        
evalAgdaExpr :: FilePath -> String -> IO()
evalAgdaExpr file expr = ioTCM file
                         Nothing
                         (cmd_compute_toplevel False expr)



interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys) = x:y:(interleave xs ys)

usage :: IO ()
usage = do
            putStrLn "Usage: agda-helper file.agda                  # interactive shell"
            putStrLn "       agda-helper file.agda -t expression    # infer type of expression"
            putStrLn "       agda-helper file.agda expression       # evaluate expression"
            putStrLn "\nInteractive mode commands:"
            putStrLn ":r                - reloads the agda file"
            putStrLn ":t expression     - infers the type of the agda expression in the context of the loaded agda file"
            putStrLn "expression        - evalates the agda expression in the context of the loaded agda file"
main :: IO ()
main = do
            
            args <- getArgs
            case args of 
                [] -> usage
                "-h":_ -> usage
                "--help":_ -> usage
                [agdaFile] -> do
                                loadAgdaFile agdaFile
                                loop $ agdaFile
                (agdaFile:"-t":rest) -> do
                                putStrLn ("loading " ++ agdaFile)
                                loadAgdaFile agdaFile
                                putStrLn "\n--------------------[ Output ]--------------------\n"
                                getExprType agdaFile $ init $ concat $ interleave rest $ repeat " "
                (agdaFile:rest) -> do
                                putStrLn ("loading " ++ agdaFile)
                                loadAgdaFile agdaFile
                                putStrLn "\n--------------------[ Output ]--------------------\n"
                                evalAgdaExpr agdaFile $ init $ concat $ interleave rest $ repeat " "


            
            
    where


    loop :: String -> IO ()
    loop agdaFile = do
        putStrLn $ "\n" ++ agdaFile ++ ":"
        line <- getLine
        continue <- eval agdaFile line
        when continue $ loop agdaFile
    
    eval :: String -> String -> IO Bool
    eval _ "exit" = return False
    eval file expr = if (expr /= "") && (ord (head expr) == 4) then return False
                     else do
                result <- (try :: IO () -> IO (Either SomeException ())) $ choose file $ splitWhiteSpace expr
                case result of
                    Left ex -> putStrLn "agda did not like that"
                    Right val -> return ()
                    
                return True

    splitWhiteSpace :: [Char] -> [[Char]]
    splitWhiteSpace "" = []
    splitWhiteSpace (':':'r':_) = [":r"]
    splitWhiteSpace (':':'t':rest) = [":t", rest]
    splitWhiteSpace str = [str]

    choose :: String -> [String] -> IO ()
    choose file [":r"] = loadAgdaFile file
    choose file [":t", expr] = getExprType file expr
    choose file [a] = evalAgdaExpr file a
    choose file []   = return ()
    choose _ x = putStrLn $ "This shouldn't happen:" ++ show x
