(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let leaf x = Node(Empty, x, Empty) 

let test_tree = Node(Node(leaf 0 , 2 ,Empty), 5, Node(leaf 6, 7, leaf 11))
let nice_tree = Node(Node(leaf 0 , 2, leaf 100), 5, Node(leaf 6, 7, leaf 11))

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror tree =
  match tree with
  |Empty -> Empty
  |Node(l, x, d) -> Node(mirror d, x, mirror l)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec height tree =
  match tree with
  |Empty -> 0
  |Node(Empty, x, Empty) -> 0
  |Node(l, x ,d) -> 1 + max (height l) (height d)

let rec size tree =
  match tree with
  |Empty -> 0
  |Node(Empty, x, Empty) -> 1
  |Node(l, x, d) -> 1 + size l + size d

(*----------------------------------------------------------------------------*]
 Funkcija [follow directions tree] tipa [direction list -> 'a tree -> 'a option]
 sprejme seznam navodil za premikanje po drevesu in vrne vozlišče do katerega 
 vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v drevesu
 vrne rezultat kot [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)

type direction = Left | Right

let rec follow = ()

(*----------------------------------------------------------------------------*]
 Funkcija [prune directions tree] poišče vozlišče v drevesu [t] glede na 
 navodila, ter izbriše poddrevo, ki se začne v izbranem vozlišču.

 Opozorilo: Pri uporabi [Some Node(l, x, r)] se OCaml pritoži, saj to razume
            kot [(Some Node)(l, x, r)], zato pravilno postavite potrebne 
            oklepaje.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)

let rec prune = ()

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo [tree] v novo drevo, ki vsebuje
 podatke drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f tree =
  match tree with
  |Empty -> Empty
  |Node(l, x, d) -> Node(map_tree f l, f x, map_tree f d)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree tree =
  let rec aux acc tree =
    match tree with
    |Empty -> acc
    |Node(l, x, d) -> aux  (x :: acc) l @ aux [] d
  in aux [] tree

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec urejen seznam =
  match seznam with
  |[] -> true
  |x :: [] -> true
  |x :: y :: [] ->
    if x <= y then true
    else false
  |x :: y :: xs ->
    if x <= y then
      urejen(y :: xs)
    else
      false

let is_bst tree = urejen (list_of_tree tree)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec insert value tree =
  match tree with
  |Empty -> leaf value
  |Node(Empty, x, Empty) ->
    if value >= x then
      Node(leaf x, value, Empty)
    else
      Node(leaf value, x, Empty)
  |Node(left, x, right) ->
    if value >= x then
      Node(left, x, insert value right)
    else
      Node(insert value left, x ,right)

let rec member value tree = 
  match tree with
  |Empty -> false
  |Node(left, x, right) ->
    if value = x then true
    else if value > x then member value right
    else member value left


(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
         funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let  member2 value tree = List.mem value (list_of_tree tree)

(*----------------------------------------------------------------------------*]
 Funkcija [bst_of_list] iz seznama naredi dvojiško iskalno drevo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)
let rec remove p list =
  match list with
  | [] -> []
  | x :: xs when (p = x) -> remove p xs
  | x :: xs -> x :: remove p xs


let rec uredi list =
  match list with
  | [] -> []
  | x ::[] -> [x]
  | x :: y :: xs ->
    let  minimum = min list 
    in minimum :: uredi (remove minimum list)

let rec bst_of_list = ()

(*----------------------------------------------------------------------------*]
 Funkcija [tree_sort] uredi seznam s pomočjo pretvorbe v bst.

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)

let rec tree_sort = ()

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let rec succ = ()

let rec pred = ()

(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
   # delete 7 test_tree;;
   - : int tree =
   Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
   Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let rec delete = ()

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Druga možnost pri brisanju podatkov je, da spremenimo tip s katerim
 predstavljamo drevo. Definirate nov tip fantomskega drevesa, ki poleg podatka,
 levega in desnega poddrevesa hrani še dodatno informacijo o stanju [state], ki 
 je bodisi [Exists] če je vozlišče še prisotno in pa [Ghost] če je vozlišče v 
 drevesu izbrisano in ga upoštevamo le še kot delitveno vozlišče.
 Še vedno predpostavljamo, da imajo drevesa obliko BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type state = Exists | Ghost

type 'a phantom_tree = unit

(*----------------------------------------------------------------------------*]
 Funkcija [phantomize] tipa ['a tree -> 'a phantom_tree] navadnemu drevesu
 priredi ekvivalentno fantomsko drevo.
 Funkcija [kill x ptree] izbriše element [x] v fantomskem drevesu tako, da 
 njegovo stanje nastavi na [Ghost].
 Predpostavite lahko, da v drevesu ni ponovitev elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)

let rec phantomize = ()

let rec kill = ()

(*----------------------------------------------------------------------------*]
 Funkcija [unphantomize] tipa ['a phantom_tree -> 'a tree] fantomskemu drevesu 
 priredi navadno drevo, ki vsebuje zgolj vozlišča, ki še obstajajo. Vrstni red
 vozlišč v končnem drevesu ni pomemben.

 Namig: Lahko uporabite vmesni prehodom na drugo podatkovno strukturo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)

let rec unphantomize = ()