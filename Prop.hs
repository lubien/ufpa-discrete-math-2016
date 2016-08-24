module Prop where

import qualified Prelude as P
import Prelude hiding ((&&), (||), not, any, all)

data Prop = Prop
  { name :: String
  , text :: String
  , value :: Bool
  }

-- Show
instance Show Prop where
  show (Prop _ _ value) = show value

-- Equals
instance Eq Prop where
  (Prop _ _ x) == (Prop _ _ y) = (x == y)

-- Not
not :: Prop -> Prop
not (Prop a b c) =
  let
    name =
      "!(" ++ a ++ ")"

    text =
      "nao (" ++ b ++ ")"

    value =
      P.not c
  in
    Prop name text value

-- And
infix 1 &&
(&&) :: Prop -> Prop -> Prop
(Prop a b c) && (Prop x y z) =
  let
    name =
      "(" ++ a ++ " && " ++ x ++ ")"

    text =
      "(" ++ b ++ " e " ++ y ++ ")"

    value =
      c P.&& z
  in
    Prop name text value

-- Or
infix 2 ||
(||) :: Prop -> Prop -> Prop
(Prop a b c) || (Prop x y z) =
  let
    name =
      "(" ++ a ++ " || " ++ x ++ ")"

    text =
      "(" ++ b ++ " ou " ++ y ++ ")"

    value =
      c P.|| z
  in
    Prop name text value

-- Implication
infix 3 ==>
(==>) :: Prop -> Prop -> Prop
(Prop a b c) ==> (Prop x y z) =
  let
    name =
      "(" ++ a ++ " -> " ++ x ++ ")"

    text =
      "(" ++ b ++ " implica " ++ y ++ ")"

    value =
      (P.not c) P.|| z
  in
    Prop name text value

-- Equivalence
infix 4 <=>
(<=>) :: Prop -> Prop -> Prop
(Prop a b c) <=> (Prop x y z) =
  let
    name =
      "(" ++ a ++ " <-> " ++ x ++ ")"

    text =
      "(" ++ b ++ " equivale a " ++ y ++ ")"

    value =
      c == z
  in
    Prop name text value

-- Get name
get_name :: Prop -> String
get_name (Prop name _ _) =
  name

-- Get text
get_text :: Prop -> String
get_text (Prop _ text _) =
  text

-- To Bool
to_bool :: Prop -> Bool
to_bool (Prop _ _ value) =
  value

-- Any
any :: (Prop -> Bool) -> [Prop] -> Bool
any _ [] =
  False
any f (p:xs) =
  if (f p)
     then True
     else any f xs

-- All
all :: (Prop -> Bool) -> [Prop] -> Bool
all _ [] =
  True
all f (p:xs) =
  if (f p)
     then all f xs
     else False

-- Debug
debug :: Prop -> String
debug (Prop name text value) =
    "Prop {\n" ++
    "  value = " ++ (show value) ++ "\n" ++
    "  name  = " ++ (show name) ++ ",\n" ++
    "  text  = " ++ (show text) ++ ",\n" ++
    "}"

-- Tests

p = Prop "P" "vacas voam" False
q = Prop "Q" "choveu hoje" True

main =
  do
    --putStrLn $ debug $ not ((p || q) ==> (q && p))
    -- putStrLn $ debug $ (p && q) <=> (q && p)
    -- putStrLn $ show $ all (\x -> (get_name x) == "Foo") [(Prop "Foo" "" True), (Prop "" "" True)]
    putStrLn $ debug $ (not p) ==> p
    putStrLn $ debug $ (not q) ==> p
