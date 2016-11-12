let inc counter = let x = !counter in counter := x + 1; x 
let push a x = x := a::(!x)
  
let normalize vect =
  let vect = Array.copy vect in
  let l = ref [] in
  let counter = ref 0 in
  for i = 0 to Array.length vect - 1 do
    let n' = vect.(i) in
    try
      let (name,id) = List.find (fun (n,_) -> n=n') (!l) in
      vect.(i) <- id
    with Not_found ->
      let n = inc counter in
      vect.(i) <- n;
      push (n',n) l;      
  done;
  vect
    
let rule110 x y z =
  match (x,y,z) with
    (false,false,false) -> false
  | (false,false,true) -> true
  | (false,true,false) -> true
  | (false,true,true) -> true
  | (true,false,false) -> false
  | (true,false,true) -> true
  | (true,true,false) -> true
  | (true,true,true) -> false

let fresh =
  let counter = ref 2 in
  fun () -> inc counter      
    
let nECA110 =
  fun vect ->
    assert (Array.length vect = 15);
    let c = normalize vect in  
    if c.(7) = c.(6) || c.(7) = c.(5) then
      if rule110 (c.(2) = c.(1)) (c.(7) = c.(6)) (c.(12) = c.(11)) then vect.(6) else vect.(5)
    else if c.(6) = c.(5) || c.(5) = c.(4) then
      if c.(7) = c.(2) || c.(7) = c.(12)
      then vect.(7)
      else
	if c.(2) = c.(12) then vect.(2)
	else fresh ()
    else vect.(7)

let init vect =
  Random.init 0;
  let cur = ref (fresh ()) in 
  let l = Array.length vect in
  let res = Array.make (5 * l) 0 in
  for i = 0 to l - 1 do
    if Random.bool () then cur := fresh ();
    let p = 5 * i in
    res.(p) <- 0;
    res.(p+1) <- 1;
    res.(p+2) <- if vect.(i) then 1 else 0;
    res.(p+3) <- !cur;
    res.(p+4) <- fresh ()
  done;
  res
    
let next vect rule =
  let l = Array.length vect in
  let c = Array.make 15 0 in
  let res = Array.copy vect in
  for i = 0 to l - 1 do
    for j = 0 to 14 do
      c.(j) <- vect.((l+i+j-7) mod l)
    done;
    res.(i) <- rule c
  done;
  res

(* UNUSED *)    
let print_ppm gen w h =
  Printf.printf "P6\n%d %d\n255\n" w h;
  for y = 0 to h do
    for x = 0 to w do
      let (r,g,b) = gen x y in
      Printf.printf "%c%c%c" (char_of_int r) (char_of_int g) (char_of_int b)
    done;
    Printf.printf "\n"
  done
    
let plot rule vect iter =
  let l = Array.length vect in
  let cur = ref vect in
  Printf.printf "P6\n%d %d\n255\n" (l/5) iter;
  let plot_line vect =
    for i = 0 to (l / 5) - 1 do
      let x = vect.((i*5) + 2) in
      let y = vect.((i*5) + 3) in
      let (r,g,b) =
	if x = 0 then (0,0,0) else
	  (255,(y/255) mod 255,y mod 255)
      in
      Printf.printf "%c%c%c" (char_of_int r) (char_of_int g) (char_of_int b)
    done;
    Printf.printf "\n"; 
  in
  for n = 0 to iter - 1 do
    plot_line !cur;
    cur := next (!cur) rule
  done;
  plot_line !cur
    
let startECA len =
  let res = Array.make len false in
  for i = 0 to len - 1 do
    res.(i) <- Random.bool ()
  done;
  res
    
let start = init (startECA 250)
  
let main = plot nECA110 start 250

