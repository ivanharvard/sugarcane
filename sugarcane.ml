let cARRAY_SIZE = 5 ;;

type plot = Dirt | Water | Sugarcane ;;

let create_array (n : int) (fill : 'a): 'a array =
  let rec create_array' (n : int) 
                        (acc : 'a list) : 'a array =
    if n = 0 then Array.of_list (List.rev acc)
    else create_array' (n - 1) (fill :: acc)
  in
  create_array' (n * n) [] ;;

let list_of_array (a : 'a array) : 'a list =
  let rec list_of_array' (a : 'a array) (n : int) : 'a list =
    if n = Array.length a then []
    else a.(n) :: list_of_array' a (n + 1) in
  list_of_array' a 0 ;;

let print_plot (p : plot) : unit =
  match p with
  | Dirt -> print_string "_"
  | Water -> print_string "W"
  | Sugarcane -> print_string "S" ;;

let print_array (a : 'a array) : unit =
  let lst_of_a = list_of_array a in
  let a_size = cARRAY_SIZE in
  let rec print_list (lst : 'a list) (n : int) : unit =
    if n mod a_size = 0 then print_newline () ;
    match lst with
    | [] -> ()
    | hd :: tl -> print_plot hd ; print_string " " ; print_list tl (n + 1) in
  print_list lst_of_a 0 ;;

let count_sugarcane (a : 'a array) : int = 
  let lst_of_a = list_of_array a in
  let rec count_sugarcane' (lst : 'a list) (n : int) : int =
    match lst with
    | [] -> 0
    | hd :: tl -> if hd = Sugarcane then 1 + count_sugarcane' tl (n + 1)
                  else count_sugarcane' tl (n + 1) in
  let count = count_sugarcane' lst_of_a 0 in
  Printf.printf "Sugarcane: %d\n" count ;
  count ;;

let place_water (a : 'a array) (x : int) (y : int): unit =
  if a.(y * cARRAY_SIZE + x) <> Dirt then failwith "placewater: attempted placing water on non-dirt plot" ;
  a.(y * cARRAY_SIZE + x) <- Water ;;

let place_sugarcane (a : 'a array): unit =
  let place_valid x y =
    if x >= 0 && x < cARRAY_SIZE && y >= 0 && y < cARRAY_SIZE && a.(y * cARRAY_SIZE + x) <> Water
    then a.(y * cARRAY_SIZE + x) <- Sugarcane
  in
  for y = 0 to cARRAY_SIZE - 1 do
    for x = 0 to cARRAY_SIZE - 1 do
      if a.(y * cARRAY_SIZE + x) = Water then begin
        place_valid (x - 1) y;  (* left *)
        place_valid (x + 1) y;  (* right *)
        place_valid x (y - 1);  (* above *)
        place_valid x (y + 1)   (* below *)
      end
    done
  done

let place_pattern (a : plot array) (init_x, init_y : int * int) (num_x : int) (num_y : int) : unit =
  let x = ref init_x in
  let y = ref init_y in
  while !y < cARRAY_SIZE do
    while !x < cARRAY_SIZE do
      a.(!y * cARRAY_SIZE + !x) <- Water;
      x := !x + num_x;
    done;
    x := !x mod cARRAY_SIZE;
    y := !y + num_y;
  done;
  place_sugarcane a ;
  ignore (count_sugarcane a) ;;

let find_best_patterns (a : plot array) : (int * int * int * int * int) list =
  let best_patterns = ref [] in
  let max_sugarcane = ref 0 in
  for init_x = 0 to cARRAY_SIZE - 1 do
    for init_y = 0 to cARRAY_SIZE - 1 do
      for num_x = 1 to cARRAY_SIZE - 1 do
        for num_y = 1 to cARRAY_SIZE - 1 do
          let a_copy = Array.copy a in
          place_pattern a_copy (init_x, init_y) num_x num_y;
          place_sugarcane a_copy;
          let num_sugarcane = count_sugarcane a_copy in
          if num_sugarcane > !max_sugarcane then begin
            max_sugarcane := num_sugarcane;
            best_patterns := [(init_x, init_y, num_x, num_y, num_sugarcane)];
          end else if num_sugarcane = !max_sugarcane then
            best_patterns := (init_x, init_y, num_x, num_y, num_sugarcane) :: !best_patterns
        done
      done
    done
  done;
  !best_patterns ;;

let print_quintuple_list (lst : (int * int * int * int * int) list) : unit =
  let rec print_quintuple_list' (lst : (int * int * int * int * int) list) : unit =
    match lst with
    | [] -> ()
    | (init_x, init_y, num_x, num_y, num_sugarcane) :: tl ->
      Printf.printf "((%d, %d), %d, %d, %d)\n" init_x init_y num_x num_y num_sugarcane;
      print_quintuple_list' tl in
  print_quintuple_list' lst ;;

(* Tests *)

let a = create_array cARRAY_SIZE Dirt ;;
place_water a (cARRAY_SIZE / 2) (0) ;;
place_sugarcane a ;;
print_array a ;;
ignore (count_sugarcane a) ;;
Printf.printf "\n" ;;

let b = create_array cARRAY_SIZE Dirt ;;
place_pattern b (0,0) 2 2 ;;
print_array b ;;
Printf.printf "\n" ;;

let c = create_array cARRAY_SIZE Dirt ;;
place_pattern c (0,0) 3 1 ;;
print_array c ;;
Printf.printf "\n" ;;

let best_plot = find_best_patterns a ;;
print_quintuple_list (best_plot) ;;

let d = create_array cARRAY_SIZE Dirt ;;  
place_pattern d (1, 0) 2 2 ;;
print_array d ;; 

let e = create_array cARRAY_SIZE Dirt ;;  
place_pattern d (0, 0) 2 2 ;;
print_array d ;; 