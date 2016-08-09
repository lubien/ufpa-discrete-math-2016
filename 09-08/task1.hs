main = do
  print [k | n <- [1, 2..100], let k = n * 2]
