namespace b1d

module MainWindow = 
    open System
    open Gtk

    let mkMenuBar menus =
                let mb = new MenuBar()
                List.iter (fun (name,entries) -> 
                               let m = new Menu()
                               let mi = new MenuItem(name:string)
                               mi.Submenu <- m
                               List.iter (fun (sname,func) ->
                                   let sm = new MenuItem(sname : string)
                                   sm.Activated.Add(fun _ -> func ())
                                   m.Append(sm)
                                   ) entries
                               mb.Append mi
                               ) menus 
                mb
        

    type game = {
            step : unit -> unit
            loadState : string -> unit
            saveState : string -> unit
            blit : (int -> int -> (float * float * float) -> unit) -> unit
            size : unit -> int * int
        }
    
    type MyWindow() as this = 
        inherit Window("MainWindow")
            do  let menus = [("File",
                                [("Load",this.OnLoad);
                                 ("Save",this.OnSave)
                                 ("Exit",Application.Quit)]);
                             ("Render",
                                [("Next screen",this.Screen)])
                            ]
                let mb = mkMenuBar menus    
                this.SetDefaultSize(800, 600)                
                this.DeleteEvent.AddHandler(fun o e -> this.OnDeleteEvent(o, e))                  
                let vbox = new VBox(false,2)
                vbox.PackStart(mb,false,false,0u)
                this.Add(vbox)
                let sw = new ScrolledWindow()
                sw.SetPolicy(Gtk.PolicyType.Always,Gtk.PolicyType.Always)
                let vp = new Gtk.Viewport()
                let da = new DrawingArea()
                da.DoubleBuffered <- true
                da.ExposeEvent.AddHandler(fun o e -> this.OnDaExposeEvent(o,e))  
                this.KeyReleaseEvent.AddHandler
                    (fun o e  ->
                        match e.Event.Key with
                        | Gdk.Key.q -> Application.Quit ()
                        | Gdk.Key.z -> this.zoomIn ()
                        | Gdk.Key.x -> this.zoomOut ()
                        | Gdk.Key.space -> this.Screen()
                        | _ -> ())                
                vp.Add(da)
                sw.Add(vp)
                vbox.Add(sw)
                this.ShowAll()

                                                                      
        let game =
            let sizex = 256
            let sizey = 256

            let states = Array2D.create sizex sizey (0,(0.0,0.0,0.0))
            let crow = ref 0
            let next () = (!crow + 1) % sizey
            let cur () = !crow
            let cycle () = 
                crow := next () 
            
            let lastid = ref 0
            let lastcol = ref (0.0,0.0,0.0)
            let rnd = new System.Random()
            let newid () = 
                let lid,(r,g,b) = !lastid,!lastcol
                let inc =  1.0 / 100000.0
                let (r1,g1,b1) = (r+inc,g+inc,b+inc) in
                let sh = rnd.NextDouble()
                let ncol = if (g1 > 1.0) then (if r1 > 1.0 then (0.0,0.0,0.0) else (r1,0.0,0.0)) else (r,g1,b1)
                let nid = lid+1
                lastid := nid
                lastcol := ncol
                (nid,ncol)

            let obsid col =
                let nid = !lastid + 1 in
                lastid := nid
                (nid,col)
                                           
            for i = 0 to sizex - 1 do
                states.[i,cur()] <- newid ()

            let put x p = 
                for i = 0 to Array.length p - 1 do
                    states.[(x+i)%sizex,cur ()] <- p.[i]

            let red = obsid (1.0,0.0,0.0)
            let yellow = obsid (1.0,1.0,0.0)
            let purple = obsid (1.0,0.0,1.0)

            let r a b = [|a;b;a;a|]
            let l a b = [|a;a;b;a|]            
            let s a = 
                let b = newid ()
                let c = newid ()
                [|a;b;a;c;a|]

            put 0 (s yellow)
            put 30 (s red)
            put 20 (r red yellow)
            put 50 (s purple)

            put 180 (l purple red)

            let rule0 ctx =
                let groups = Seq.groupBy fst ctx 
                let matches = Seq.map (fun (id,l) -> (Seq.length l,Seq.head l)) groups
                let ordered = Seq.sortBy fst matches
                let l = Seq.toList ordered
                match l with
                    | [(4,a)] -> a
                    | [(1,a);(3,b)] -> a
                    | [(2,a);(2,b)] -> newid ()
                    | [(1,a);(1,b);(2,c)] -> c
                    | [(1,_);(1,_);(1,_);(1,_)] -> newid ()                
                    | _ -> failwith "unexpected internal error in game rule application (use the source!)\n"

            {   step = 
                   fun () ->
                    let ctx = Array.create 4 (0,(0.0,0.0,0.0))                                            
                    for i = 0 to sizex - 1 do
                        for j = 0 to 3 do ctx.[j] <- states.[(i-1+j+sizex)%sizex,cur ()]
                        states.[i,next ()] <- rule0 ctx
                    cycle ()                                
                
                loadState = fun s -> // stub
                    printf "load state: %s\n" s

                saveState = fun s -> // stub
                    printf "save state %s\n" s

                blit = fun rect ->
                    Array2D.iteri (fun i j (_,c) ->   
                                        let cx = ((i + (j/2) % sizex) + sizex) % sizex
                                        rect cx j c) states
                                                         
                size = fun () -> (sizex,sizey) } // stub 
            
        
        let stepcnt = ref 0

        let zoom = ref 0.0

        let idleHnd = new GLib.IdleHandler(fun () -> 
                        if !zoom = 0.0 then this.initZoom()
                        let y = fst (game.size ())
                        game.step()
                        stepcnt := (!stepcnt + 1) % y
                        if !stepcnt = y - 1
                        then this.QueueDraw (); false
                        else true)
                                                     
        do ignore (GLib.Idle.Add(idleHnd))        


        let size (win : Gdk.Window) = let rx,ry = ref 0,ref 0 in win.GetSize(rx,ry); !rx,!ry

        member this.initZoom () = 
            let (x,y) = size(this.GdkWindow) 
            let (gx,gy) = game.size()
            zoom := (float x) / (float gx)
        
        member this.Screen () =
            ignore (GLib.Idle.Add(idleHnd))

        member this.zoomIn () = 
            zoom := min (!zoom + 0.5) 10.0
            this.QueueDraw()

        member this.zoomOut () =
            zoom := max (!zoom - 0.5) 0.5
            this.QueueDraw()

        member this.OnLoad () =
            use dlg = new Gtk.FileChooserDialog("load file",null,Gtk.FileChooserAction.Open,
                                                [| box "Cancel"; 
                                                   box ResponseType.Cancel; 
                                                   box "Open"; 
                                                   box ResponseType.Accept |]) in
            if dlg.Run() = int ResponseType.Accept 
            then game.loadState (dlg.Filename.ToString())
            dlg.Hide()

        member this.OnSave () =
            use dlg = new Gtk.FileChooserDialog("save file",null,Gtk.FileChooserAction.Save,
                                                [| box "Cancel"; 
                                                   box ResponseType.Cancel; 
                                                   box "Save"; 
                                                   box ResponseType.Accept |])
            if dlg.Run() = int ResponseType.Accept 
            then game.saveState (dlg.Filename.ToString())
            dlg.Hide()
                              
        member this.OnDeleteEvent(o, e : DeleteEventArgs) = 
            Application.Quit()
            e.RetVal <- true

        member this.OnDaExposeEvent(o, e) =
            let da = o :?> DrawingArea
            let win = da.GdkWindow
            let (sx,sy) = game.size ()
            use cr = Gdk.CairoHelper.Create(win)
            let v = Array.create (3 * sx * sy) (byte 255)
            let rect x y (r,g,b) = 
                let z = (y * 3 * sx) + (3 * x)
                let conv v = byte (255.0 * v)
                v.[z] <- conv r
                v.[z+1] <- conv g
                v.[z+2] <- conv b
            game.blit rect
            use gpb = new Gdk.Pixbuf(v,Gdk.Colorspace.Rgb,false,8,sx,sy,3*sx,null)
            let factor = !zoom
            let fsx,fsy = (float sx,float sy)
            da.SetSizeRequest(int (factor * fsx),int (factor * fsy))            
            //gpb.ScaleSimple(sx,sy,Gdk.InterpType.Tiles)
            cr.Scale(factor,factor)
            Gdk.CairoHelper.SetSourcePixbuf(cr,gpb,0.0,0.0)
            cr.Paint()
           