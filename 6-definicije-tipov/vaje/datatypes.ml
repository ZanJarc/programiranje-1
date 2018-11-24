

type color =
  |Red
  |Blue
  |Yellow
  |RBG of int * int * int

type 'a list =
  | Empty (* [] *)
  | Cons of 'a * 'a list (* x :: xs *)

type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree
  
