module Prop where

import qualified Prelude as P
import Prelude hiding ((&&), (||), not)

data Prop = Prop
  { name :: String
  , text :: String
  , value :: Bool
  }

instance Show Prop where
  show (Prop _ _ value) = show value

instance Eq Prop where
  (Prop _ _ x) == (Prop _ _ y) = (x == y)

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

infix 4 ==>
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

-- Debug

debug :: Prop -> String
debug (Prop name text value) =
  "Prop { " ++ name ++ ", " ++ text ++ ", " ++ (show value) ++ " }"

-- Tests

p = Prop "P" "Vacas voam" False
q = Prop "Q" "Choveu hoje" True

main =
  do
    putStrLn $ show $ debug $ (p || q) ==> (q && p)
