-- Escreva uma função que receba um caractere e retorne Verdadeiro se o caractere for 'a' e Falso caso contrário.

teste :: Char -> Bool
teste char =
  char == 'a'

main = do
  putStrLn $ show $ teste 'b'
  putStrLn $ show $ teste 'a'
