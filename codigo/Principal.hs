import System.IO 
import Control.Exception
import System.IO.Error 
import System.Process
import Control.Monad (when)
import Text.Printf

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

opcoesTelaInicial :: [String]
opcoesTelaInicial = ["Entrar como gestor", "Entrar como funcionário", "Entrar como cliente", "Sair"]

doMainScreen :: Integer -> [Char] -> IO ()
doMainScreen cursor action | action == "\ESC[B" = mainScreen ((cursor+1) `mod` 4)
                                                    | action == "\ESC[A" && cursor /= 0 = mainScreen (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = mainScreen 3
                                                    | action == "\ESC[C" = changeMainScreen cursor
                                                    | action == "\ESC[D" = putStrLn("Sair")
                                                    | otherwise = mainScreen cursor

changeMainScreen :: Integer -> IO()
changeMainScreen cursor                          | cursor == 0 = do return() 
                                                 | cursor == 1 = do return()            
                                                 | cursor == 2 = do return() 
                                                 | cursor == 3 = do return()

showSimpleScreen :: [String] -> Integer -> Integer -> IO()
showSimpleScreen [] cursor contador = return ()
showSimpleScreen (o:os) cursor contador = do
   if contador == cursor
      then 
      putStrLn("->" ++ o)
   else
      putStrLn("  " ++ o)
   showSimpleScreen os cursor (contador+1)

mainScreen :: Integer -> IO ()
mainScreen cursor = do
   
   system "clear"
   putStrLn("Bem-vindo à farmácia Corona Pharm!")
   putStrLn("Como desejas acessar?\n")
   putStrLn("\n|| Utilize os direcionais do teclado para mover o cursor ||\n")
   showSimpleScreen opcoesTelaInicial cursor 0
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doMainScreen cursor action

opcoesTelaGestor :: [String]
opcoesTelaGestor = ["Cadastrar produto", "Cadastrar funcionário", "Atualizar preço", "Visualizar produtos", "Visualizar clientes", "Visualizar vendas"]

opcoesTelaFuncionario :: [String]
opcoesTelaFuncionario = ["Cadastrar cliente", "Cadastrar venda", "Visualizar clientes", "Visualizar produtos", "Visualizar lista de suas vendas"]

opcoesTelaCliente :: [String]
opcoesTelaCliente = ["Visualizar produtos", "Comprar produto"]

opcoesTelaVisualizarProdutos :: [String]
opcoesTelaVisualizarProdutos = ["Visualizar por sintoma", "Visualizar por existentes no estoque"]

run :: IO ()
run = do
   {catch (iniciar) error;}
   where
      iniciar = do
      {
        --  arq <- openFile "arquivos/Produtos.txt" ReadMode;
        --  dados <- hGetLine arq;
        --  hClose arq;

        --  arq2 <- openFile "arquivos/Clientes.txt" ReadMode;
        --  dados2 <- hGetLine arq2;
        --  hClose arq2;

         mainScreen 0;
         return ()
      }
      error = ioError 

main :: IO ()
main = do
   run