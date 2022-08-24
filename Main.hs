module Main (main) where

import Control.Concurrent
import Control.DeepSeq
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

imprime :: String -> IO ()
imprime path = do
  file <- readFile path
  putStrLn "================================="
  putStr "Voce Selecionou: "
  putStrLn path
  putStrLn "================================="
  putStrLn file

deposito :: Float -> IO ()
deposito value = do
  saldo <- readFile "saldo.txt"
  let saldoFloat = (read saldo :: Float)
  let saldoatualizado = saldoFloat + value
  let saldostring = (show saldoatualizado :: String)
  let depositValueS = (show value :: String)
  let conc = "(+)"
  let depositValueString = conc ++ depositValueS ++ "\n"
  extrato <- appendFile "extrato.txt" depositValueString
  putStrLn ""
  rnf saldo `seq` writeFile "saldo.txt" saldostring

saque :: Float -> IO ()
saque value = do
  saldo <- readFile "saldo.txt"
  let saldoFloat = (read saldo :: Float)
  let saldoatualizado = saldoFloat - value
  let saldostring = (show saldoatualizado :: String)
  let depositValueS = (show value :: String)
  let conc = "(-)"
  let depositValueString = conc ++depositValueS++"\n"
  extrato <- appendFile "extrato.txt" depositValueString
  putStrLn ""
  rnf saldo `seq` writeFile "saldo.txt" saldostring

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Hello"
  putStrLn "World"
  putStrLn "================================="
  putStrLn "Banco: Thiago Vasconcelos Braga"
  putStrLn "================================="
  menu

menu = do
  putStrLn "1. Saldo: "
  putStrLn "2. Extrato: "
  putStrLn "3. Depósito: "
  putStrLn "4. Saque: "
  putStrLn "5. Fim: "
  op <- getLine
  let opcao = (read op :: Int)
  case opcao of
    1 -> imprime "saldo.txt"
    2 -> imprime "extrato.txt"
    3 -> do
      putStrLn ("Digite o valor do depósito: ")
      value <- getLine
      let valueFloat = (read value :: Float)
      deposito valueFloat
    4 -> do
      putStrLn ("Digite o valor do saque: ")
      valueS <- getLine
      let saqueValue = (read valueS :: Float)
      saque saqueValue
    5 -> putStrLn "Teste5"
    _ -> putStrLn "A opção digitada é invalida"
  if opcao /= 5
    then menu
    else putStrLn "Obrigado por utilizar nosso banco"
