(*Modul.ml*)

(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks (*ko so vsi elementi porabljeni *)
    | _, [] -> List.rev (List.rev chunk :: chunks) (*če je list porabljen ampak zadnji chunk še ni napolnjen do konca *)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst (*ko se trenutni chunk napolni*)
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

(* f x = x |>  f *)
let string_of_list string_of_element sep lst = (*vzame seznam, vsak njegov element pretvori v string z funkcijo string_of_element in vse te stringe združi v en string z ločilom sep *)
  lst |> List.map string_of_element |> String.concat sep (*String.concat vzame sep-arator in list stringov in jih združi v en string, kjer so vsi stringi staknjeni skupaaj z sep-aratorjem*)

let string_of_nested_list string_of_element inner_sep outer_sep =(*seznam seznamov pretvori v seznam stringov ločenih z inner in nato v velik string ločen z outer *)
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row = (*spremeni row(array) v seznam seznmov dolžine 3 ki jih potem zlepi skupaj v en velik seznam kjer sse elementi podseznamov držijo skupaj elementi seznama pa so ločeni z | *)
  let string_of_cells =
    row |> Array.to_list |> chunkify 3   (*ker so vse vrstice dolge točno 9, in drugega ne boš uporabljal*)
    |> string_of_nested_list string_of_cell "" "│"  (* za string_of_element uporabiš string_of_cell*)
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =(*grid spremeni v seznam, ta seznam pa v sezname po 3, vrstico razdeli z | in nato vsako trojico vrstic razdeli z divider-jom*)
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = (*Iz grida, ti vrne vrstico row_ind *)
grid.(row_ind)

let rows grid = (* vrne ti seznam vrstic(arreyev) po vrsti*)
List.init 9 (get_row grid) (*init n is [f 0; f 1; ...; f n], evaluated left to right.*)


let get_column (grid : 'a grid) (col_ind : int) = (*ti vrne stolpec iz grida  *)
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)(* ti vrne seznam stolpcev(arreyev) *)

let levi_zgornji_kot (box_ind : int) = 
  (box_ind - (box_ind mod 3)), (box_ind mod 3) * 3

let get_box (grid : 'a grid) (box_ind : int) = (*vrneš array elementov iz ene škatle*)
  let a = fst (levi_zgornji_kot box_ind)
  and b = snd (levi_zgornji_kot box_ind) in
  let r = ref [||] in 
  for i = a to a + 2 do
    for j = b to b + 2 do
      r := Array.append !r [|grid.(i).(j)|]
    done
  done;
  !r
let boxes grid = List.init 9 (get_column grid)(* ti vrne seznam boxov(arreyev) *)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = (*na vsako vrstici samo uporabiš map in jiz zložiš v nov grid*)
Array.init 9 (fun row_ind -> Array.map f grid.(row_ind))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid (*naredi kopijo grida*)

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str = (* preslika vsak element stringa v celico v seznamu s funkcijo cell_of_char*)
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char
(*String.get s i = s.(i) *)
(*filter_map f l applies f to every element of l, filters out the None elements and returns the list of the arguments of the Some elements*)
let grid_of_string cell_of_char str =
  let grid = (*iz stringa naredi grid*)
    str |> String.split_on_char '\n' (*String v seznam stringov, ločenih glede na ločilo '\n' torej ti string razdeli glede na vrstice*)
    |> List.map (row_of_string cell_of_char) (*iz vsake vrstice naredi podseznam, kjer so vsi elementi char preslikani z cell_of_char *)
    |> List.filter (function [] -> false | _ -> true)(* izloci prazne vrstice *)
    |> List.map Array.of_list |> Array.of_list (*vsakega od podseznamov spremeni v array in cel seznam spremeni v array (torej smo dobili tip grid)*)
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let string_of_int_option (a : int option) = match a with (*dodefiniramo si funkcijo "string_of_cell"*)
| None -> " "
| Some i -> string_of_int i

let print_problem problem : unit = 
print_grid string_of_int_option problem.initial_grid 

let problem_of_string str = (*izpiše ti iz stringa problem grida tipa  int option*)
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0')) (*Char.code ti vrne numerično ASCII vrednost znaka*)
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid string_of_int solution

let rec normalna_resitev = function (* (int list -> bool) da se res vsaka številka pojavi točno enkrat v seznamu*)
| [] -> true
| list -> let lista, listb = 
  List.partition (fun x -> x = (List.length list)) list in
    if List.length lista = 1 then normalna_resitev listb else false

let normalen_problem seznam = (* 'a option list -> bool, preveri, da se noben Some n ne pojavi več kot enkrat*)
let rec aux acc = function 
| [] -> true
| None :: lst -> aux acc lst
| Some n :: lst -> if List.mem (Some n) acc then false
else aux (Some n :: acc) lst
in aux [] seznam

let je_res_podmnozica (problem : problem) (solution : solution) = (* preveriš, da so vsa števila problem-a vsebovana na istih mestih kot v solution*)
  let fun_comp row_ind col_ind prob_elem acc = match prob_elem with 
  | None -> true && acc
  | Some y -> (solution.(row_ind).(col_ind) = y) && acc 
  in 
    foldi_grid fun_comp problem.initial_grid true

let is_valid_solution problem solution = (*Preverimo, da je so v vseh sklopih točno vsa števila med 1 in 9 in da solution nikjer ne overida vrednosti problema*)
  let skupaj = (rows solution) @ (columns solution) @ (boxes solution) in 
  let vsi_digiti = skupaj |> List.map Array.to_list |> List.map normalna_resitev |> List.fold_left ((&&)) true in
  vsi_digiti && je_res_podmnozica problem solution

let is_valid_problem grid = (*preverimo, če je sudoku sploh rešljiv*)
  let skupaj = (rows grid) @ (columns grid) @ (boxes grid) in 
  skupaj |> List.map Array.to_list |> List.map normalen_problem |> List.fold_left ((&&)) true
    
(*Solver.ml*)

type available = { loc : int * int; possible : int list } 

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : problem; current_grid : int option grid; available_grid: available grid } (* Dopolnil si z available_grid *)

let list_to_grid list = (* spremeni seznam v grid in deluje tako na tipe problem kot tipe solution*)
  list |> chunkify 9 |> List.map Array.of_list |> Array.of_list

let print_state (state : state) : unit = (* ti naprinta current_grid int option grid*)
  print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid


let preveri_vsebovanost row col (grid : int option grid) k = (*Iz koordinat točke v gridu, grid-a in števila k preveri, če je  število k že v rowu coloumnu ali boxu točke*)
  let box = (row - row mod 3) + (col - col mod 3)/3 in (*iz koordinat v gridu ti pove index box-a *)
  let vsi_ze_zavzeti = [get_row grid row; get_column grid col; get_box grid box] in
  let booll = vsi_ze_zavzeti |> List.map Array.to_list |> List.concat |> List.mem (Some k) in
 if booll then false else true (*List.concat ti elemente iz seznamov iz seznama seznamov da vse v en seznam*)

let generate_list zacetek konec =
    let rec aux acc z k =
      if z > k then
        acc
      else
        aux (z :: acc) (z + 1) k
    in aux [] zacetek konec

let vsi_mozni_elementi row col (grid : int option grid) (n : int option) = (*Iz koordinate točk v gridu, grid-a in vrednosti točke v gridu, vrne type available za to točko*)
  let seznam_1_do_9 = generate_list 1 9 in 
  let vsi_se_nevsebovani = List.filter (fun i -> preveri_vsebovanost row col grid i) seznam_1_do_9 in 
    match n with
    | None -> {loc = (row, col); possible = vsi_se_nevsebovani}
    | Some _ -> {loc = (row, col); possible = []}


let naredi_available_grid (grid : int option grid) =(* ustvari available_grid iz current_grid-a *)
  let foldi row col (n : int option) (acc : available list) = acc @ [(vsi_mozni_elementi row col grid n)] in
    list_to_grid (foldi_grid foldi grid [])

let match_possibles (available : available) (n : int option) = (*pomatcha vrednosti available.possible v pare*)
  if n = None then match available.possible with 
    | [] -> (None, true)
    | x :: [] -> (Some x, false)
    | _ -> (None, false)
  else (n, false)

(* Iz current_grida in available_grida napise current_grid z izpolnjenimi rešitvami *)
let zapolni_prazne (grid : int option grid) (available_grid : available grid) =
  let folding_function row col (n : int option) acc =
    fst acc @ [fst (match_possibles (available_grid.(row).(col)) n)], 
    snd acc || snd (match_possibles (available_grid.(row).(col)) n)
  in 
    list_to_grid (fst (foldi_grid folding_function grid ([], false))), snd (foldi_grid folding_function grid ([], false))

(* Iz current_grida in available_grida vrne seznam s koordinatami točk, ki imajo le dve prosti mesti*)
let seznam_dveh_moznih (grid : int option grid)  (available_grid : available grid) =
  let fun_comp row col (cell : int option) acc = match cell with
  | None -> if List.length (available_grid.(row).(col).possible) = 2 then (row, col) :: acc  else acc 
  | _ -> acc
  in foldi_grid fun_comp grid []


type response = Solved of solution | Unsolved of state | Fail of state

let initialize_state (problem : problem) : state =
  { current_grid = copy_grid problem.initial_grid; problem; available_grid = naredi_available_grid problem.initial_grid }

let validate_state (state : state) (napacno : bool) : response =
  let unsolved = (* preveri, če obstaja še kakšno nerešeno polje torej polje vrednosti None*)
    Array.exists (Array.exists Option.is_none) state.current_grid (* Option.is_none vrne true samo če je tip 'a option, vrednosti konstruktorja None *)
  in
  if unsolved then (*Dopolnil sem funkcijo še z opcijo, da preveri, če je Fail state *)
      if (is_valid_problem state.current_grid) && (not napacno)  then Unsolved state
      else Fail state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = map_grid Option.get state.current_grid in (*Option.get od Some x ti vrne x*)
    if is_valid_solution state.problem solution then Solved solution
    else Fail state

     
let rec update_and_validate_state (state : state) =  (*počisti stanje, dokler ne dobi vsaj enega polja z natanko dvema možnostima*)
  let nov_grid, napacno = zapolni_prazne (copy_grid state.current_grid) (copy_grid state.available_grid) in
    let nov_available_grid = naredi_available_grid nov_grid in 
      let posodobljen_state = {problem = state.problem; current_grid = nov_grid; available_grid = nov_available_grid} in 
        match validate_state posodobljen_state napacno with (* za obravnavo robnih problemov *)
        | Solved solution -> Solved solution
        | Fail fail -> Fail fail
        | Unsolved state -> if (List.length (seznam_dveh_moznih nov_grid nov_available_grid) > 0) 
                            then Unsolved state else update_and_validate_state posodobljen_state

          
let vstavi_novo_vrednost grid row col vrednost = 
  let posodobljen_grid = copy_grid grid in
    (posodobljen_grid).(row).(col) <- vrednost;
  posodobljen_grid

let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
    let seznam = seznam_dveh_moznih state.current_grid state.available_grid in 
    match seznam with
    | [] -> None
    | (row, col) :: _ -> 
    let hipoteza = Array.of_list state.available_grid.(row).(col).possible in
        Some (
              {problem = state.problem;
              current_grid = (vstavi_novo_vrednost (state.current_grid) row col (Some hipoteza.(0)));
              available_grid = (copy_grid state.available_grid)}, 
              {problem = state.problem;
              current_grid = (vstavi_novo_vrednost (state.current_grid) row col (Some hipoteza.(1)));
              available_grid = (copy_grid state.available_grid)}
              )
      

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match update_and_validate_state state with (* namesto validate_state smo tukaj zamenjali s funkcijo, ki stanje tudi zozi*)
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : problem) =
  problem |> initialize_state |> solve_state

(*Main.ml*)

let read_problem filename =
  let channel = open_in filename in
  let str = really_input_string channel (in_channel_length channel) in
  close_in channel;
  problem_of_string str

let find_solution problem =
  let before = Sys.time () in
  let solution = solve_problem problem in
  let after = Sys.time () in
  let elapsed_time = after -. before in
  (solution, elapsed_time)

let display_solution = function
  | Some solution ->
      Printf.printf "Končna rešitev:\n";
      print_solution solution
  | None -> Printf.printf "Rešitev ne obstaja.\n"

let find_and_display_solution (problem : problem) =
  Printf.printf "Rešujem:\n";
  print_problem problem;
  Printf.printf "\n%!";
  let response, elapsed_time = find_solution problem in
  display_solution response;
  Printf.printf "Čas reševanja: %f s.\n%!" elapsed_time


;;  Printexc.record_backtrace true;;


(*  Prevajaj z:
   
C:\OCaml64\usr\local\bin\ocaml-env.exe exec -- C:\OCaml64\home\Uporabnik\.opam\4.14.0+mingw64c\bin\ocamlopt.exe -g ml solver.ml main.ml -o sudoku.exe

*)




let () =
  (* Če se program sesuje, nam to izpiše klicni sklad. *)
  Printexc.record_backtrace true;
  (* Tabela sistemskih argumentov vsebuje ime klicanega programa ter argumente, ki mu sledijo *)
  Sys.argv
  (* Tabelo pretvorimo v seznam *)
  |> Array.to_list
  (* Odstranimo prvi element (ime klicanega programa), da dobimo seznam imen datotek *)
  |> List.tl
  (* Iz vsake datoteke preberemo problem *)
  |> List.map read_problem
  (* Probleme zaporedoma rešimo *)
  |> List.iter find_and_display_solution

(* Če domačo nalogo rešujete prek spletnega vmesnika, ki ne podpira branja datotek,
   lahko delovanje preizkušate prek spodnjega programa. *)

let () = "
┏━━━┯━━━┯━━━┓
┃483│921│657┃
┃967│3 5│821┃
┃251│876│493┃
┠───┼───┼───┨
┃548│132│976┃
┃729│ 64│ 38┃
┃136│798│ 45┃
┠───┼───┼───┨
┃372│689│514┃
┃814│253│769┃
┃695│417│382┃
┗━━━┷━━━┷━━━┛"
  |> problem_of_string
  |> find_and_display_solution 
