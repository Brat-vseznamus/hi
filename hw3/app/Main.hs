module Main
  where
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Exts (fromList)
import HW3.Action (HIO (runHIO), HiPermission (..))
import HW3.Base (HiError, HiValue)
import HW3.Evaluator (eval)
import HW3.Parser (getErrorMessage, parse)
import HW3.Pretty (prettyValue)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input ->
                 do
                   let result = parse input in
                     case result of
                       (Left error') -> do
                          outputStrLn $ getErrorMessage error'
                          loop
                       (Right expr) -> do
                          evaluation <- liftIO
                            (try 
                              (runHIO 
                                (eval expr) 
                                (fromList 
                                  [
                                    AllowRead, 
                                    AllowWrite,
                                    AllowTime
                                  ]))
                            :: IO (Either SomeException (Either HiError HiValue)))
                          case evaluation of
                            (Left e) ->
                              outputStrLn $ show e
                            (Right (Left error')) -> do
                              outputStrLn $ show error'
                            (Right (Right value)) -> do
                              outputStrLn $ show $ prettyValue value
                          loop


