teste x y =
  (x + y) - y == x

displayCase x y = do
  putStrLn $ "teste(" ++ (show x) ++ ", " ++ (show y) ++ ")"
  putStrLn $ show $ teste x y
  putStrLn $ ""

main = do
  displayCase 1 2
  displayCase 1.0 2.0
  displayCase (2.0^200) (3.0^300)
  displayCase (1.0) (5.0^1234)
