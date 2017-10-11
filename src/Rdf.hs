module Rdf
    (
      unode2, triple, lnode2 , plainl2,
      Node(..), Triple(..), LValue(..)
    ) where

unode2 :: [Char] -> Node
plainl2 :: [Char] -> LValue
data Triple = Triple !Node !Node !Node
triple a b c = Triple a b c
unode2 x = UNode x
plainl2 x = PlainL x

data LValue =
  PlainL ![Char]
--  | PlainLL !T.Text !T.Text
--  | TypedL !T.Text  !T.Text


data Node =
  NodeId(Int)
  | UNode ![Char]
--  | BNode !String
--  | BNodeGen !Int
  | LNode !LValue

lnode2 x = LNode x

