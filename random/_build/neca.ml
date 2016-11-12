(* Generic cellular automata *)

type rule = { radius : int;
	      fn : (int -> int) -> int }

let step rule source dest =
  let size = Array.length source in
  for i = 0 to size - 1 do
    let reader j =
      Array.get source ((size + i + j - (rule.radius / 2)) mod size)
    in
    Array.set dest i (rule.fn reader) 
  done
    
let simulate blit_fn rule init iter =
  let one = ref (Array.copy init) in
  let two = ref (Array.copy !one) in
  for i = 1 to iter do
    blit_fn i !one;
    step rule !one !two;
    let x = !one in
    one := !two;
    two := x
  done;
  !one

(* Graphics code *)    
        
let graph_run color_fn scale rule init iter =  
  let blit_fn iteration array =
    let square x y size color =
      Graphics.set_color color;
      Graphics.fill_rect x y size size 
    in
    let curx = ref 0  in
    for n = 0 to (Array.length array) - 1 do
      square (!curx) (scale * (iter - iteration)) scale (color_fn iteration array.(n));
      curx := !curx + scale;
    done
  in
  Graphics.auto_synchronize false;
  let _ = simulate blit_fn rule init iter in ();
  Graphics.auto_synchronize true

(** Nominal *)
    
let normalise x dest =
  let idx = ref 0 in
  Array.set dest 0 !idx;
  for i = 1 to (Array.length dest) - 1 do
    let found = ref false in
    let j = ref 0 in
    while (not !found) && (!j < i) do
      if x i = x !j then
	begin
	  found := true;
	  Array.set dest i dest.(!j)
	end
      else j := !j + 1
    done;
    if not (!found) then
      begin
	idx := !idx + 1;
	Array.set dest i !idx
      end
  done;
  !idx
    
let pseudo_random_answer seed input max =
  let l = Array.length input in
  let x = Array.create (l + 1) input.(0) in
  for i = 0 to l - 1 do
    Array.set x i input.(i)
  done;
  Array.set x l seed;
  Random.full_init x;
  Random.int max

let find i source =
  let res = ref 0 in
  let found = ref false in
  while (not !found) && (!res < Array.length source) do
    if source.(!res) = i then found := true
    else res := !res + 1	
  done;
  if !found then !res else raise Not_found      
    
let pseudo_random_rule seed radius min_int =
  let normalised = Array.create radius 0 in
  let counter = ref min_int in
  { radius = radius;
    fn =
      (fun x ->
	let max = normalise x normalised in
	let ans = pseudo_random_answer seed normalised (max + 2) in
	if ans > max then
	  begin
	    let i = !counter in
	    counter := i + 1;
	    i
	  end
(*	else find ans normalised)}   (* 314 3 *) *)
	else x (find ans normalised))}

(* main program *)

let true_random () = Random.self_init (); Random.int (max_int lsr 32)

let true_random_m m = Random.self_init (); Random.int m

type status =
  { radius : int;
    size : int;
    iter : int;
    init_seed : int;    
    min_new : int;
    seed : int;
    init_algorithm : string}
    
let new_status () =
  let seed = true_random () in
  let init_seed = true_random () in
  let radius = 4 in
  let iter = 60 in
  let min_new = radius + 6 in
  let size = 60 in
  let init_algorithm = "random" in
  { radius = radius;
    size = size;
    iter = iter;
    init_seed = init_seed;
    min_new = min_new;
    seed = seed;
    init_algorithm = init_algorithm;}

let read_status str =
  let h = Hashtbl.create 10 in
  List.iter
    (fun x ->
      let [name;value] = List.map String.trim (Str.split (Str.regexp "=") x) in
      Hashtbl.add h name value)
    (Str.split (Str.regexp ",") str);
  { radius = int_of_string (Hashtbl.find h "radius");
    size = int_of_string (Hashtbl.find h "size");
    iter = int_of_string (Hashtbl.find h "iter");
    init_seed = int_of_string (Hashtbl.find h "init_seed");
    min_new = int_of_string (Hashtbl.find h "min_new");
    seed = int_of_string (Hashtbl.find h "seed");
    init_algorithm = Hashtbl.find h "init_algorithm" }
    
let write_status st =
  Printf.sprintf "radius=%d,size=%d,iter=%d,init_seed=%d,min_new=%d,seed=%d,init_algorithm=%s"
    st.radius st.size st.iter st.init_seed st.min_new st.seed st.init_algorithm

let random_init_algorithm status =
  Random.init status.init_seed;
  Array.init status.size (fun x -> Random.int status.min_new)
    
let (init_algorithms,init_algorithms_names) =
  let h = Hashtbl.create 10 in
  let keys = ref [] in
  List.iter (fun (key,value) -> Hashtbl.add h key value; keys := key::!keys)
    [ ("random",random_init_algorithm);
      ("short_quasiperiodic",
       fun status ->
	 Random.init status.init_seed;
	 Array.init status.size (fun x -> if 0 = Random.int 7 then Random.int status.min_new else x mod status.min_new));
      ("mid_quasiperiodic",
       fun status ->
	 Random.init status.init_seed;
	 Array.init status.size (fun x -> if 0 = Random.int 2 * status.min_new then Random.int status.min_new else x/(2*status.min_new) mod status.min_new))
    ];
  (h,List.rev !keys)

let get_init status =
  let init_algorithm =
    try Hashtbl.find init_algorithms status.init_algorithm
    with Not_found -> random_init_algorithm
  in
  init_algorithm status

let get_rule status =
  pseudo_random_rule status.seed status.radius status.min_new
    
let _ =
  let save_file = "saved.csv" in
  let scale = ref 16 in
  let status_stack = Stack.create () in
  let status = new_status () in
  Stack.push status status_stack;
  
  Graphics.open_graph "";
  Graphics.resize_window (status.size * !scale) (status.iter * !scale);
  
  let finish = ref false in
  while not (Stack.is_empty status_stack || !finish) do
    let status = Stack.top status_stack in
    Printf.printf "%s\n%!" (write_status status);
    let init = get_init status in
    let rule = get_rule status in 
    graph_run (fun iter n -> if n < status.min_new then (255/ status.min_new*n) lsl 16 else n) !scale rule init status.iter;
    let next = ref false in
    while not (!next) do
      next := true;
      let k = (Graphics.wait_next_event [Graphics.Key_pressed]).Graphics.key in
      match k with
	'q' -> finish := true
      | 'i' -> Stack.push { status with init_seed = true_random () } status_stack
      | 'r' -> Stack.push { status with seed = true_random () } status_stack
      | '+' ->
	 begin
	   scale := max (!scale / 2) 1;
	   Stack.push { status with
	     size = status.size * 2;
	     iter = status.iter * 2 } status_stack
	 end
(*      | '-' ->
	 begin
	   scale := 2 * !scale;
	   Stack.push { status with
	     size = max 10 (status.size / 2);
	iter = max 10 (status.iter / 2) } status_stack; 
	 end *)
      | 's' -> (let f = open_out_gen [Open_creat;Open_append] 0o644 save_file in
		try Printf.fprintf f "%s\n" (write_status status);
		    close_out f;
		    Printf.printf "saved\n%!"
		with e -> (close_out f; Printf.printf "cannot save file\n%!"))
      | 'l' -> (let f = open_in save_file in
		try
		  while true do
		    Stack.push (read_status (input_line f)) status_stack
		  done
		with End_of_file -> close_in f)
      | 'e' -> ignore (Stack.pop status_stack)
      | 'm' ->
	 let rec fn l = match l with
	     ([]|[_]) -> List.hd init_algorithms_names
	   | x::y::xs -> if x == status.init_algorithm then y else fn (y::xs)
	 in Stack.push { status with init_algorithm = fn init_algorithms_names } status_stack
      | _ -> next := false
    done
  done;
  Graphics.close_graph ();











  

(* 
   interesting seeds
   
   radius 3:
   4013428 

   radius 4:
   771830373
   897449693
   radius: 4 size: 120 iter: 200 init_seed: 963768767 min_new: 20 seed: 322801429
   radius: 4 size: 100 iter: 300 init_seed: 593650138 min_new: 5 seed: 1056540924
   radius: 4 size: 100 iter: 300 init_seed: 40758431 min_new: 5 seed: 555943506
   radius: 4 size: 640 iter: 640 init_seed: 490466269 min_new: 20 seed: 993230758 maxi collisioni   
   radius: 4 size: 60 iter: 100 init_seed: 660890908 min_new: 10 seed: 82036816 come fa a creare i puntini rossi?
   radius: 4 size: 120 iter: 200 init_seed: 364743017 min_new: 7 seed: 367016667
   radius: 4 size: 120 iter: 200 init_seed: 486399216 min_new: 7 seed: 522187553   
   radius: 4 size: 120 iter: 200 init_seed: 764641619 min_new: 7 seed: 879074512

   radius 7:
   140134102
*)
