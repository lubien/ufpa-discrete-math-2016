-- Escrever uma função que recebe uma string e retorna True se a string for "Olá" e False caso contrário. Isto pode ser feito, especificando cada elemento da string no padrão de lista (por exemplo, 'o': 'i': []).

teste :: String -> Bool
teste str =
  str == "Olá"

main = do
  putStrLn $ show $ teste "Não"
  putStrLn $ show $ teste "Olá"

