//#r "mypb.dll"

let sizex = 200
let sizey = 200
let ncols = 5 
let density = 0.7 
let delay = 0.0001 // in milliseconds

let apply rule orig dest =
    for i = 0 to sizex - 1 do
        for j = 0 to sizey - 1 do
            (dest : int [,]).[i,j] <- rule orig (i,j)
        done
    done

let neighbourhood (x,y) = [(x-1,y-1);(x,y-1);(x+1,y-1);(x-1,y);(x+1,y);(x-1,y+1);(x,y+1);(x+1,y+1)]

let neighbours v (x,y) =  
    List.filter (fun (x,y) -> x >= 0 && x <= (sizex - 1) && y >= 0 && y <= (sizey - 1)) (neighbourhood (x,y)) 

let fullNeighbours v (x,y) = (x,y)::(neighbours v (x,y))

let rnd = System.Random ()

let (newid,colormap,lastid,newidcol) =
    let counter = ref 0 in
    let colormap = ref (Map.ofList [0,(255,255,255)]) in
    ((fun () -> 
        let v = !counter + 1 in 
        counter := v;
        let r = rnd.Next 256 in
        let g = rnd.Next 256 in
        let b = rnd.Next 256 in
        colormap := (!colormap).Add (v,(r,g,b)); 
        v),
     (fun x -> ((!colormap).[x])),
     (fun () -> !counter),
     (fun c -> let v = ! counter + 1 in
                counter:= v;
                colormap := (!colormap).Add (v,c);
                v))

let majorityOld v (x,y) =
    neighbours v (x,y)
    |> Seq.map (fun (x,y) -> Array2D.get v x y)
    |> Seq.groupBy id
    |> Seq.map (fun (x,s) -> (x,Seq.length s))
    |> Seq.sortByDescending snd
    |> fun x ->
        if Seq.length x = 1 then fst (Seq.head x) 
        else if snd (Seq.head x) = snd (Seq.head (Seq.tail x)) 
             then newid () 
             else fst (Seq.head x)

let democracy v (x,y) =
    neighbours v (x,y)
    |> List.map (fun (x,y) -> Array2D.get v x y)
    |> List.groupBy id
    |> List.map (fun (x,s) -> (x,Seq.length s))
    |> List.sortByDescending snd
    |> fun x ->
        let threshold = 6 in
        let protectMinority s = if snd (List.head s) < threshold 
                                then fst (List.head s) 
                                else let s' = List.rev s in 
                                     if snd (List.head s') = snd (List.head (List.tail s'))
                                     then newid ()
                                     else fst (List.head x)                                         
        if List.length x = 1 then fst (List.head x)
        else if snd (List.head x) = snd (List.head (List.tail x)) 
             then newid () 
             else protectMinority x

let tomNew v (x,y) =
    neighbours v (x,y)
    |> List.map (fun (x,y) -> Array2D.get v x y)
    |> List.groupBy id
    |> List.map (fun (x,s) -> (x,List.length s))
    |> List.sortBy snd
    |> Array.ofList
    |> fun a -> 
        let f i = fst a.[i-1] in
        match Array.map snd a with
        | [|8|] -> f 1
        | [|1;7|] -> f 2
        | [|2;6|] -> f 1 
        | [|3;5|] -> f 1 
        | [|4;4|] -> newid ()
        | [|1;1;6|] -> f 3 
        | [|1;2;5|] -> f 3 
        | [|1;3;4|] -> newid ()
        | [|2;2;4|] -> f 3
        | [|2;3;3|] -> f 1
        | [|1;1;1;5|] -> f 4
        | [|1;1;2;4|] -> f 4
        | [|1;1;3;3|] -> newid ()
        | [|1;2;2;3|] -> newid ()
        | [|2;2;2;2|] -> newid ()
        | [|1;1;1;1;4|] -> newid ()
        | [|1;1;1;2;3|] -> newid ()
        | [|1;1;2;2;2|] -> newid ()
        | [|1;1;1;1;1;3|] -> f 6
        | [|1;1;1;1;2;2|] -> newid ()
        | [|1;1;1;1;1;1;2|] -> f 7
        | [|1;1;1;1;1;1;1;1|] -> newid ()
        | _ -> v.[x,y] // on borders


let tom k v (x,y) =
    neighbours v (x,y)
    |> List.map (fun (x,y) -> Array2D.get v x y)
    |> List.groupBy id
    |> List.map (fun (x,s) -> (x,List.length s))
    |> List.sortBy snd
    |> Array.ofList
    |> fun a -> 
        let f i = fst a.[i-1] in
        match Array.map snd a with
        | [|8|] -> f 1
        | [|1;7|] -> f 2
        | [|2;6|] -> f 1 
        | [|3;5|] -> f 1 
        | [|4;4|] -> k
        | [|1;1;6|] -> f 3 
        | [|1;2;5|] -> f 3 
        | [|1;3;4|] -> k
        | [|2;2;4|] -> f 3
        | [|2;3;3|] -> f 1
        | [|1;1;1;5|] -> f 4
        | [|1;1;2;4|] -> f 4
        | [|1;1;3;3|] -> k
        | [|1;2;2;3|] -> k
        | [|2;2;2;2|] -> k
        | [|1;1;1;1;4|] -> k
        | [|1;1;1;2;3|] -> k
        | [|1;1;2;2;2|] -> k
        | [|1;1;1;1;1;3|] -> f 6
        | [|1;1;1;1;2;2|] -> k
        | [|1;1;1;1;1;1;2|] -> f 7
        | [|1;1;1;1;1;1;1;1|] -> k
        | _ -> v.[x,y] // on borders


let lifeLike v (x,y) =
    neighbours v (x,y)
    |> Seq.map (fun (x,y) -> Array2D.get v x y)
    |> Seq.filter (fun x -> x <> 0)
    |> Seq.groupBy id
    |> Seq.map (fun (x,s) -> (x,Seq.length s))
    |> fun seq -> 
        let numNeighbours colour = match Map.tryFind colour (Map.ofSeq seq) with None -> 0 | Some n -> n in
        let living = v.[x,y] <> 0 in
        let die = 0 in
        if living 
        then 
            let colour = v.[x,y] in
            let n = numNeighbours colour in
                if n = 2 || n = 3 
                then colour
                else die   
        else let generators = Seq.filter (fun (x,n) -> n = 3) seq in
                match Seq.length generators with    
                | 0 -> die
                | 1 -> fst (Seq.head seq)
                | _ -> newid ()

let densityInit (v : int[,]) numids density =
    let ids = Array.zeroCreate numids in
    for i = 0 to numids - 1 do
        ids.[i] <- newid ()
    done
    for i = 0 to sizex - 1 do
        for j = 0 to sizey - 1 do
            if rnd.NextDouble () < density then v.[i,j] <- ids.[rnd.Next numids] 
        done
    done 

let randomInit (v : int[,]) numids =
    let ids = Array.zeroCreate numids in 
    for i = 1 to numids - 1 do
        ids.[i] <- newid ()
    done
    for i = 0 to sizex - 1 do   
        for j = 0 to sizey - 1 do
            v.[i,j] <- ids.[rnd.Next(numids)]
        done
    done 

let biasedInit (v : int[,]) numids =
    let ids = Array.zeroCreate numids in 
    for i = 1 to numids - 1 do
        ids.[i] <- newid ()
    done
    for i = 0 to sizex - 1 do   
        for j = 0 to sizey - 1 do
            if rnd.Next(2) = 0 
            then v.[i,j] <-  i * numids / sizex 
            v.[i,j] <- ids.[rnd.Next(numids)]
        done
    done 

let blitImage v (img : System.Drawing.Bitmap) =
    Array2D.iteri (fun i j coloridx -> 
                    let (x,y,z) = colormap coloridx in
                        img.SetPixel(i,j,(System.Drawing.Color.FromArgb(x,y,z)))) v

let swap a b =
    let c = !a in 
    a := !b;
    b := c

open System
open System.Windows.Forms
open System.Drawing

let a = ref (Array2D.create sizex sizey 0) in
let b = ref (Array2D.create sizex sizey 0) in
randomInit (!a) ncols 
let k = newidcol (0,0,0)
let img = new System.Drawing.Bitmap(sizex,sizey,System.Drawing.Imaging.PixelFormat.Format24bppRgb)
blitImage !a img
let form = new Form()
let pb = new System.Windows.Forms.PictureBox()
//let pb = new PictureBoxWithInterpolationMode()
//pb.InterpolationMode <- System.Drawing.Drawing2D.InterpolationMode.NearestNeighbor
pb.Image <- img
pb.SizeMode <- PictureBoxSizeMode.StretchImage
pb.Dock <- DockStyle.Fill
form.Controls.Add(pb)
form.Size <- new Size(600,600)
let r = ref 0
let step _ =
            r := 1 + !r 
            apply (tom k) (!a) (!b)
            swap a b
            if !r % 10 = 0 then printf "iteration %d\n" !r 
            blitImage !a img
            pb.Refresh()
let t = new System.Timers.Timer() in
t.Interval = delay
form.KeyDown.Add (fun _ -> t.Enabled <- not t.Enabled)
t.Enabled <- true
t.Elapsed.Add (fun _ -> t.Stop(); step (); t.Start())
t.Start

[<STAThread>]
do
    Application.Run(form)

