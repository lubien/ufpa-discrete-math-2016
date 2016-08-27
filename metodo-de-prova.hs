import qualified Prop hiding (main)

integers = [1..100]

main =
  let
    even = \x -> x `mod` 2 == 0
    odd = \x -> (not (even x))

    prop1 = Prop.Prop "P" "0 eh par" (0 `mod` 2 == 0)

    prop2 = Prop.Prop "Q" "-301 eh impar" (not (-301 `mod` 2 == 0))

    prop3 = Prop.Prop "R" "Se a e b sao numeros inteiros, eh 6a2b eh par?" (all even [6 * (a^2) * b | a <- integers, b <-integers])

    prop4 = Prop.Prop "S" "Se a e b sao numeros inteiros, 10a + 8b + 1 é impar?" (all odd [10 * a + 8 * b + 1 | a <- integers, b <- integers])
  in
    do
      putStrLn $ Prop.debug $ prop1
      putStrLn $ Prop.debug $ prop2
      putStrLn $ Prop.debug $ prop3
      putStrLn $ Prop.debug $ prop4
