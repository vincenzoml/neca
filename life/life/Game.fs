namespace life
open System

module Game = 
    let sizex = 200
    let sizey = 200

    let inline apply rule orig dest =
        for i = 0 to sizex - 1 do
            for j = 0 to sizey - 1 do
                (dest : int [,]).[i,j] <- rule orig (i,j)
            done
        done

    let inline neighbourhood (x,y) = [(x-1,y-1);(x,y-1);(x+1,y-1);(x-1,y);(x+1,y);(x-1,y+1);(x,y+1);(x+1,y+1)]

    let inline neighbours v (x,y) =  
        List.map (fun (x,y) -> ((sizex + x)%sizex,(sizey+y)%sizey)) (neighbourhood (x,y))

    let inline fullNeighbours v (x,y) = (x,y)::(neighbours v (x,y))

    let rnd = System.Random ()

    let (newid,colormap,mkid) =
        let counter = ref 0 in
        let colormap = ref (Map.ofList [0,(255uy,255uy,255uy)]) in
        let inline mkid c = 
                let v = !counter + 1 in 
                counter := v;
                colormap := (!colormap).Add (v,c);
                v
        let inline colormap x = (!colormap).[x]
        let inline newid () =
            let r = byte (rnd.Next 256) in
            let g = byte (rnd.Next 256) in
            let b = byte (rnd.Next 256) in
            mkid (r,g,b)                     
        (newid,colormap,mkid)    

    let contextsResponses =  
        //2×3×3×3×2×4×4×2×2×2×3×3×2×3×2×2
        //= 2'985'984 possible rules
            [|([|8|],[|0;1|]);
              ([|1;7|],[|0;1;2|]);
              ([|2;6|],[|0;1;2|]);
              ([|3;5|],[|0;1;2|]);
              ([|4;4|],[|0|]);
              ([|1;1;6|],[|0;3|]);
              ([|1;2;5|],[|0;1;2;3|]);
              ([|1;3;4|],[|0;1;2;3|]);
              ([|2;2;4|],[|0;3|]);
              ([|2;3;3|],[|0;1|]);
              ([|1;1;1;5|],[|0;4|]);
              ([|1;1;2;4|],[|0;3;4|]);
              ([|1;1;3;3|],[|0|]);
              ([|1;2;2;3|],[|0;1;4|]);
              ([|2;2;2;2|],[|0|]);
              ([|1;1;1;1;4|],[|0;5|]);
              ([|1;1;1;2;3|],[|0;4;5|]);
              ([|1;1;2;2;2|],[|0|]);
              ([|1;1;1;1;1;3|],[|0;6|]);
              ([|1;1;1;1;2;2|],[|0|]);
              ([|1;1;1;1;1;1;2|],[|0;7|]);
              ([|1;1;1;1;1;1;1;1|],[|0|])|]

    let admissible rule = 
        let s = Seq.zip {1..(Seq.length rule)} rule in
        Seq.length rule = 22 && Seq.forall2 (fun i v ->  Seq.contains v  (snd contextsResponses.[i])) 
            {0..(Seq.length rule - 1)} rule

    let decode (rule : int[]) =
        if not (admissible rule) then failwith "Non-admissible rule"
        fun  v (x,y) ->
        neighbours v (x,y)
        |> List.map (fun (x,y) -> Array2D.get v x y)
        |> List.groupBy id
        |> List.map (fun (x,s) -> (x,List.length s))
        |> List.sortBy snd
        |> Array.ofList
        |> fun a -> 
            let f i = fst a.[i-1] in
            let r i =
                //printf "i,rule.[i]: %d,%d\n" i rule.[i]
                if 0 = rule.[i] then newid () else f rule.[i]
            match Array.map snd a with
            | [|8|] -> r 0
            | [|1;7|] -> r 1
            | [|2;6|] -> r 2
            | [|3;5|] -> r 3
            | [|4;4|] -> r 4
            | [|1;1;6|] -> r 5
            | [|1;2;5|] -> r 6 
            | [|1;3;4|] -> r 7
            | [|2;2;4|] -> r 8 
            | [|2;3;3|] -> r 9 
            | [|1;1;1;5|] -> r 10
            | [|1;1;2;4|] -> r 11
            | [|1;1;3;3|] -> r 12
            | [|1;2;2;3|] -> r 13
            | [|2;2;2;2|] -> r 14
            | [|1;1;1;1;4|] -> r 15
            | [|1;1;1;2;3|] -> r 16
            | [|1;1;2;2;2|] -> r 17
            | [|1;1;1;1;1;3|] -> r 18
            | [|1;1;1;1;2;2|] -> r 19
            | [|1;1;1;1;1;1;2|] -> r 20
            | [|1;1;1;1;1;1;1;1|] -> r 21
            | _ -> v.[x,y]

    
    let vin  = [|1;2;2;1;0;0;3;1;3;1;4;4;0;4;0;5;5;0;6;0;7;0|]
    let news = [|1;2;1;2;0;3;1;2;3;1;4;3;0;1;0;0;0;0;6;0;7;0|]    
    let tom  = [|1;2;1;1;0;3;3;0;3;1;4;4;0;0;0;0;0;0;6;0;7;0|]
    let isla = [|1;2;1;0;0;3;1;2;3;1;4;3;0;1;0;0;0;0;6;0;7;0|]
                  
    let inline stable v (x,y) =
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
            | [|2;6|] -> f 2
            | [|3;5|] -> f 2
            | [|4;4|] -> newid ()
            | [|1;1;6|] -> f 3 
            | [|1;2;5|] -> f 3  
            | [|1;3;4|] -> f 3
            | [|2;2;4|] -> f 3
            | [|2;3;3|] -> newid ()
            | [|1;1;1;5|] -> f 4
            | [|1;1;2;4|] -> f 4
            | [|1;1;3;3|] -> newid ()
            | [|1;2;2;3|] -> f 4 // newid ()
            | [|2;2;2;2|] -> newid ()
            | [|1;1;1;1;4|] -> f 5 // newid ()
            | [|1;1;1;2;3|] -> f 5 // newid () // Change to f 4 for much faster and cahotic regions
            | [|1;1;2;2;2|] -> newid ()
            | [|1;1;1;1;1;3|] -> f 6
            | [|1;1;1;1;2;2|] -> newid ()
            | [|1;1;1;1;1;1;2|] -> f 7
            | [|1;1;1;1;1;1;1;1|] -> newid ()
            | _ -> v.[x,y] // on borders

    let inline lifeLike v (x,y) =
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

    let randomInit numids =
        let ids = Array.init numids (fun _ -> newid ())
        Array2D.init sizex sizey (fun i j -> ids.[rnd.Next(numids)])

    let kColsInit k = 
        let ids = Array.init k (fun _ -> newid ())
        let v = Array2D.init sizex sizey (fun _ _ -> ids.[0])
        for i = 0 to sizey - 1 do v.[0,i] <- ids.[rnd.Next(k)]
        for j = 0 to sizex - 1 do v.[j,0] <- ids.[1]
        v

    let simpleInit () =
        let k = newid () in
        let h = newid () in
        let v = Array2D.init sizex sizey (fun _ _ -> k)
        for i = 0 to 20 do for j = 0 to 10 do v.[i,j] <- h
        v

    let zeroInit () = Array2D.create sizex sizey 0

    let swap a b =
        let c = !a 
        a := !b;
        b := c

    let a = ref (simpleInit ()) 
    let b = ref (zeroInit ()) 
    let rule = ref isla

    let decodedRule = ref (decode !rule)

    let reinit () = a:= randomInit 4

    let stepCounter = ref 0

    let saveImage prefix =
        let d = new System.Drawing.Bitmap(100,100,System.Drawing.Imaging.PixelFormat.Format24bppRgb) 
        Array2D.iteri (fun x y c -> 
            let (r,g,b) = colormap c 
            d.SetPixel(x,y,Drawing.Color.FromArgb(int r,int g,int b))) !a
        d.Save (sprintf "%s_%07d.png" prefix !stepCounter)

    let loadImage (filename : string) =
        let d = new System.Drawing.Bitmap(filename) 
        let m = ref Map.empty in
        stepCounter := 0
        a := Array2D.init d.Size.Width d.Size.Height 
                (fun x y -> 
                    let c = d.GetPixel (x,y) 
                    let (r,g,b) = (c.R,c.G,c.B)
                    if (!m).ContainsKey (r,g,b)
                        then (!m).[r,g,b]
                        else let j = mkid (r,g,b) 
                             m := (!m).Add ((r,g,b),j)
                             j)
        
    let updateRule () = 
        decodedRule := decode (!rule)

    let step _ =
        stepCounter := 1 + !stepCounter;
        apply !decodedRule (!a) (!b);
        swap a b;
        //if !r % 10 = 0 then printf "iteration %d\n" !r
        //if !r % 100 = 0 then saveImage "iteration"
    let blitImage set =
        let k = colormap in
        Array2D.iteri (fun x y c -> set x y (colormap c)) !a
 