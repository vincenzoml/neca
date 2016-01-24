(* Generic cellular automata *)

type 'a rule = { radius : int;
		 fn : (int -> 'a) -> 'a }

let step rule source dest =
  let size = Array.length source in
  for i = 0 to size - 1 do
    let reader j =
      Array.get source ((size + i + j - (rule.radius / 2)) mod size)
    in
    Array.set dest i (rule.fn reader) 
  done
    
let simulate blit_fn rule init_fn size iter =
  let one = ref (Array.init size init_fn) in
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
        

let graph_run scale rule init_fn size iter =
  let color_fn (r,g,b) = Graphics.rgb r g b in
  let blit_fn i array =
    let square x y size color =
      Graphics.set_color color;
      for i = 0 to size - 1 do
	for j = 0 to size - 1 do	
	  Graphics.plot (x+i) (y+j)
	done
      done
    in
    let curx = ref 0  in
    for n = 0 to (Array.length array) - 1 do
      square (!curx) (scale * (i-1)) scale (color_fn array.(n));
      curx := !curx + scale;
    done
  in

  Graphics.open_graph "";
  Graphics.clear_graph ();
  Graphics.resize_window (size * scale) (iter * scale);  
  Graphics.auto_synchronize false;
  let x = simulate blit_fn rule init_fn size iter in
  Graphics.auto_synchronize true;
  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
  Graphics.close_graph ();
  x
    
  
    
let _ =
  Random.init 23;
  let rule = { radius = 3;
	       fn = (fun x ->
		 if ((x 0) = (x 1)) || ((x 1) = (x 2))
		 then (x 1)
		 else let (r,g,b) = (x 1) in (r,g+17 mod 255,b-13 mod 255)) } in
  
  graph_run 10 rule (fun i ->  (Random.int 20) * 25,0,0) 100 80
    
    
    
    
