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
                                [("Toggle throttling",this.ToggleThrottling);
                                 ("Toggle pause",this.TogglePause)])
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
                da.Events <- da.Events ||| Gdk.EventMask.ScrollMask
                da.ExposeEvent.AddHandler(fun o e -> this.OnDaExposeEvent(o,e))  
                this.KeyReleaseEvent.AddHandler
                    (fun o e  ->
                        match e.Event.Key with
                        | Gdk.Key.p -> this.TogglePause ()
                        | Gdk.Key.t -> this.ToggleThrottling ()
                        | Gdk.Key.q -> Application.Quit ()
                        | _ -> ())                
                da.ScrollEvent.AddHandler
                    (fun o e -> 
                        match e.Event.Direction with
                        | Gdk.ScrollDirection.Up -> this.zoomIn ()
                        | Gdk.ScrollDirection.Down -> this.zoomOut ()
                        | _ -> ())
                vp.Add(da)
                sw.Add(vp)
                vbox.Add(sw)
                this.ShowAll()
                                      
                 
        do ignore (GLib.Timeout.Add(100u,new GLib.TimeoutHandler(fun () -> this.QueueDraw();true)))       

                              
        let game =
            let sizex = 100
            let sizey = 1000

            let states = Array2D.create sizex sizey (0,(0.0,0.0,0.0))
            let crow = ref 0
            let next () = (!crow + 1) % sizey
            let cur () = !crow
            let cycle () = 
                crow := next () 
            
            let lastid = ref 0
            let lastcol = ref (0.0,0.0,0.0)
            let newid () = 
                let lid,(r,g,b) = !lastid,!lastcol
                let inc = 1.0 / 100000.0
                let (r1,g1,b1) = (r+inc,g+inc,b+inc) in
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

            let a = obsid (255.0,0.0,0.0)
            let b = obsid (255.0,255.0,0.0)
            let c = obsid (255.0,0.0,255.0)

            let p1 = [|a;b;a;a|] 
            let p2 = [|b;b;c;b|]
            let p3 = [|a;b;a;c;a|]

            put 10 p1
            put 30 p2
            put 50 p3

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

            {   step = fun () ->                    
                    for i = 0 to sizex - 1 do
                        let ctx = Array.create 4 (0,(0.0,0.0,0.0))
                        for j = 0 to 3 do ctx.[j] <- states.[(i-2+j+sizex)%sizex,cur ()]
                        states.[i,next ()] <- rule0 ctx
                    cycle ()
                
                loadState = fun s -> // stub
                    printf "load state: %s\n" s

                saveState = fun s -> // stub
                    printf "save state %s\n" s

                blit = fun rect ->
                    Array2D.iteri (fun i j (_,c) ->   
                                        let cx = ((i - (j/2) % sizex) + sizex) % sizex
                                        rect cx j c) states
                                                         
                size = fun () -> (sizex,sizey) } // stub 
            
        
        let throttling = ref true
        let paused = ref false
        let idleHnd = new GLib.IdleHandler(fun () -> 
                        game.step()
                        if not !throttling then this.QueueDraw()
                        not !paused)
        
        do ignore (GLib.Idle.Add(idleHnd))        

        let zoom = ref 1.0

        member this.zoomIn () = 
            zoom := min (!zoom + 0.5) 10.0

        member this.zoomOut () =
            zoom := max (!zoom - 0.5) 0.5

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

        member this.ToggleThrottling () = throttling := not (!throttling)
                              
        member this.TogglePause () =
            paused := not (!paused)
            if not !paused then ignore (GLib.Idle.Add(idleHnd))
                              
        member this.OnDeleteEvent(o, e : DeleteEventArgs) = 
            Application.Quit()
            e.RetVal <- true

        member this.OnDaExposeEvent(o, e) =
            let da = o :?> DrawingArea
            let win = da.GdkWindow
            let wsx,wsy = let rx,ry = ref 0,ref 0 in win.GetSize(rx,ry); !rx,!ry
            let fwsx,fwsy = float wsx,float wsy            
            let (sx,sy) = game.size ()
            let fsx,fsy = (float sx,float sy)
            da.SetSizeRequest(int (float sx * !zoom),int (float sy * !zoom))
            use cr = Gdk.CairoHelper.Create(win)
            cr.Scale(!zoom * fwsx/fsx,!zoom * fwsy/fsy)
            let rect x y (r,g,b) = 
                cr.SetSourceRGB(r,g,b) 
                cr.Rectangle(float x,float y,3.0,3.0)
                cr.Fill()
            game.blit rect
