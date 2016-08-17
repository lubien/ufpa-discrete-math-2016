-- Escreva uma função que recebe uma string e remove um espaço à esquerda se ele existir.

teste :: String -> String
teste (' ':tail) = tail
teste str = str

main = do
  putStrLn $ show $ teste "Bar"
  putStrLn $ show $ teste " Foo"

