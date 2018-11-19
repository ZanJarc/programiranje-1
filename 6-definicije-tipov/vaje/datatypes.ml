type a' option = None | Some of 'a

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
  
let has zero tree = 
  match three with
  | Empty ->
  | Node (x, left_tree, right_tree) -> 