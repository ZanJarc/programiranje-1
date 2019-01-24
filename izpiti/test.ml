(* === Prva naloga === *)

let razlika_kvadratov x y =
    (x + y) * (x + y) - (x*x + y*y)
(*===ali
let razlika_kvadratov x y =
  (x + y) ** 2.0 - (x ** 2.0 + y ** 2.0)
===*)

let uporabi_na_paru f (a,b) =
  (f a, f b)

let rec ponovi_seznam n list =
  if n <= 0 then
    []
  else
    list @ (ponovi_seznam (n - 1) list)

let rec razdeli sez =
  let rec razdeli' n_acc p_acc sez =
    match sez with
    |[] -> (List.rev n_acc, List.rev p_acc) (*List.rev seznam nam vrne obrnjen seznam*)
    |x :: xs when x < 0 -> razdeli' (x :: n_acc) p_acc xs
    |x :: xs when x > 0 -> razdeli' n_acc (x :: p_acc) xs
  in razdeli' [] [] sez

(*=== Druga naloga ===*)
type 'a drevo = Empty | Node of 'a drevo * 'a * 'a drevo

let leaf x = Node(Empty, x, Empty)

let rec padajoca value drevo =
  match drevo with
  |Empty -> []
  |Node (lt, x, rt) when x > value -> [] 
  |Node (lt, x, rt) when x < value -> 
    let left = padajoca x lt in
    let right = padajoca x rt in
    if List.length left > List.length right then
      x :: left
    else
      x :: right

let rec narascajoca value drevo =
  match drevo with
  |Empty -> []
  |Node (lt, x, rt) when x < value -> []
  |Node (lt, x, rt) when x > value -> 
    let left = narascajoca x lt in
    let right = narascajoca x rt in
    if List.length left > List.length right then
      left @ [x]
    else
      right @ [x]
  
let test1 =
  Node(
    Node(leaf 3, 10, Node(leaf 14, 13, leaf 6)),
    11,
    Node(leaf 2, 8, leaf 10)
  )


let rec monotona_pot drevo =
  match drevo with
  |Empty -> []
  |Node(lt, x, rt) ->
    let pure_left = monotona_pot lt in
    let pure_right = monotona_pot rt in
    let left_to_right = (padajoca x lt) @ [x] @ (narascajoca x rt) in
    let right_to_left = (padajoca x rt) @ [x] @ (narascajoca x lt) in
    let options = [pure_right; left_to_right; right_to_left] in
    let pick_bigger x y = if List.length x > List.length y then x else y in
    List.fold_left pick_bigger pure_left options

(*=== Tretja naloga ===*)

type 'a veriga =
    |Filter of ('a -> bool) * 'a list * 'a veriga
    |Ostalo of 'a list

(* a *)
let test =
  Filter((fun x -> x > 0), [],
  Filter((fun x -> x < 10), [],
  Ostalo []))

(* b *)
let rec vstavi x veriga =
  match veriga with
  |Ostalo (elementi) -> Ostalo (x :: elementi)
  |Filter (f, elementi, filtri) ->
    if f x then 
      Filter (f, x :: elementi, filtri)
    else
      Filter (f, elementi, vstavi x filtri)

(* c *)

let rec poisci x = function 
  | Ostalo elementi -> List.mem x elementi
  | Filter(f, elementi, filtri) ->
    if f x then (*iskali bomo le tam, kjer x lahko je, torej f(x)=True*)
      List.mem x elementi
    else
      poisci x filtri

let rec izprazni = function
  | Ostalo elementi -> (Ostalo [], elementi)
  | Filter(f, elementi, filtri) ->
    let prazni_filtri, pobrani_elementi = izprazni filtri in
    let vsi_elementi = elementi @ pobrani_elementi in
    (Filter(f, [], prazni_filtri), vsi_elementi)


let rec dodaj f veriga =
  let veriga' = Filter(f, [], veriga) in
  let prazna_veriga, elementi = izprazni veriga' in
  List.fold_left(fun v x -> vstavi x v) prazna_veriga elementi