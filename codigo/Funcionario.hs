import System.IO 
import Control.Exception
import System.IO.Error 
import System.Process
import Control.Monad (when)
import Text.Printf
 
type funcionarios = [Funcionario]                                                   
type nomeFuncionario = String
type id = String
type data = String
type salario = Double
data Funcionario = Funcionario nomeFuncionario id data salario


getNovoNomeDoFuncionario :: IO Double
getNovoNomeDeFuncionario = do 
   putStrLn("\nDigite um Nome do novo funcionário")
   hSetBuffering stdin LineBuffering
   hSetEcho stdin True
   x <- readLn
   return x

getNovoSalarioDoFuncionario :: IO Double
getNovoPesoNota = do 
   putStrLn("\nDigite um Salário para o novofuncionário")
   hSetBuffering stdin LineBuffering
   hSetEcho stdin True
   x <- readLn
   return x

getNovoIdDoFuncionario :: IO String
getNovoNomeDeFuncionario = do 
   putStrLn("\nDigite um Id do novo funcionário")
   hSetBuffering stdin LineBuffering
   hSetEcho stdin True
   x <- readLn
   return x

getNovaDataDeAdmissaoDoFuncionario :: IO String
getNovoNomeDeFuncionario = do 
   putStrLn("\nDigite um Id do novo funcionário")
   hSetBuffering stdin LineBuffering
   hSetEcho stdin True
   x <- readLn
   return x



