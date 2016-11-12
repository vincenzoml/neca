
type 'a status =
  { radius : int;
    size : int;
    iter : int;
    init_seed : int;    
    min_new : int;
    seed : int }
      
let _ =
  let f = open_in "saved.csv" in
  try
    while true do
      let s = read_status (input_line f) in
      Printf.printf "%s\n%!" (write_status s)
    done
  with End_of_file -> close_in f
    










  

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
