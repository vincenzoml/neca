type 'a rule = { radius : int;
		 fn : (int -> 'a) -> 'a }

let step rule source dest =
  let size = Array.length source in
  let input = Array.create rule.radius source.(0) in  
  for i = 0 to size - 1 do
    let start = (i - (rule.radius / 2)) mod size in
    let current = ref (if start >= 0 then start else start + size) in
    for j = 0 to rule.radius - 1 do
      Array.set input j source.(!current);
      current := (!current + 1) mod size
    done;
    let output = rule.fn (Array.get input) in
    Array.set dest i output;
  done;
  dest

let simulate rule blit_fn size init_fn iter =
  let start = Array.create size (init_fn 0) in
  for i = 1 to size - 1 do
    Array.set start i (init_fn i)
  done;    
  let current = ref start in
  let newarr = ref (Array.copy start) in
  for i = 1 to iter do
    blit_fn i !current;
    step rule !current !newarr;
    current := !newarr;
    newarr := !current
  done
    
    
let blit_fn_graphics scale color_fn x y array =
  let square x y size color =
    Graphics.set_color color;
    for i = 0 to size - 1 do
      for j = 0 to size - 1 do	
	Graphics.plot (x+i) (y+j)
      done
    done
  in
  let curx = ref x  in
  for n = 0 to (Array.length array) - 1 do
    square (!curx) y scale (color_fn array.(n));
    curx := !curx + scale;
  done
    
let _ =
  Random.init 23;
  let rule = { radius = 3;
	       fn = (fun x ->
		 if ((x 0) = (x 1)) || ((x 1) = (x 2))
		 then (x 1)
		 else let (r,g,b) = (x 1) in (r,g+17 mod 255,b-13 mod 255)) } in
  
  let size = 50 in
  let iter = 100 in
  let scale = 10 in

  let color_fn (r,g,b) = Graphics.rgb r g b in
  let init_fn i = (Random.int 20) * 25,0,0 in
  let blit_fn i arr =
    blit_fn_graphics scale color_fn 0 (scale * (i-1)) arr in  

  Graphics.open_graph "";
  Graphics.clear_graph ();
  Graphics.resize_window (size * scale) (iter * scale);

  Graphics.auto_synchronize false;

  simulate rule blit_fn size init_fn iter;
  
  Graphics.auto_synchronize true;

  Printf.printf "press enter to exit\n%!";
  let _ = input_line stdin in
  Graphics.close_graph ()


    
