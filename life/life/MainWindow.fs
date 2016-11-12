namespace life

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
        

    type MyWindow() as this = 
        inherit Window("MainWindow")
            do  let menus = [("File",
                                [("Load",this.OnLoad);
                                 ("Save",this.OnSave)
                                 ("Exit",Application.Quit)]);
                             ("Render",
                                [("Toggle throttling",this.ToggleThrottling);
                                 ("Toggle pause",this.TogglePause)]);
                             ("Rules",
                                [("Rule editor",fun () -> this.ShowRuleSelector())])]
                let mb = mkMenuBar menus    
                this.SetDefaultSize(800, 600)                
                this.DeleteEvent.AddHandler(fun o e -> this.OnDeleteEvent(o, e))                  
                let vbox = new VBox(false,2)
                vbox.PackStart(mb,false,false,0u)
                this.Add(vbox)
                let da = new DrawingArea()
                vbox.Add(da)
                da.ExposeEvent.AddHandler(fun o e -> this.OnDaExposeEvent(o,e))
                //ignore (GLib.Idle.Add(fun () -> s.ShowAll();false))
                this.ShowAll()
                               
          
        do ignore (GLib.Timeout.Add(1000u,new GLib.TimeoutHandler(fun () -> this.QueueDraw();true)))       

        let throttling = ref true
        let paused = ref false
        let idleHnd = new GLib.IdleHandler(fun () -> 
                        Game.step()
                        if !throttling then this.QueueDraw()
                        not !paused)
        
        do ignore (GLib.Idle.Add(idleHnd))           

        let ruleselector = ref (null : Window)

        let mkRuleSelector () =
            let win = new Gtk.Window("Rule editor") in
            let lbl = new Gtk.Label()           
            let updateLbl () = lbl.Text <- Array.fold (fun s i -> s + i.ToString()) "rule: " (!Game.rule)
            updateLbl()
            let tbl = new Table(10u,22u,false)
            let widgets = Array2D.create 10 22 (null : Widget)           
            let add w i j = 
                tbl.Attach(w,uint32 (i+1),uint32 (i+2),uint32 (j+1),uint32 (j+2))
                widgets.[i,j] <- w         
            let untoggled(w : Button) =
                w.Label <- w.Data.["value"] :?> string
            let toggled(row,w : Button) =
                w.Label <- "[" + (w.Data.["value"] :?> string) + "]"
                for i = 0 to 9 do                   
                    if widgets.[i,row] <> null 
                    then 
                        if (widgets.[i,row].GetType()) = w.GetType()
                        then
                            let w1 = widgets.[i,row] :?> Button 
                            if (w1.Data.["value"] :?> string) <> (w.Data.["value"] :?> string)
                            then untoggled w1            
            let clickHnd (b : Button) idx value e = 
                toggled (idx,b)
                (!Game.rule).[idx] <- value
                Game.updateRule()
                updateLbl()
            Array.iteri (fun j (ctx,resps) ->
                let b = new Button("0")
                b.Data.Add("value","0") 
                b.Clicked.Add (clickHnd b j 0)
                add b 0 j
                //tbl.Attach(b,0u,1u,uint32 j,uint32 (j+1))
                Array.iteri (fun i value ->                     
                    if Array.contains (i+1) resps
                    then let b = new Button()
                         let txt = value.ToString()
                         b.Data.Add("value",txt)
                         if (!Game.rule).[j] = i+1 
                         then toggled(j,b)
                         else untoggled(b)
                         b.Clicked.Add (clickHnd b j (i+1))
                         add b (i+1) j
                         //tbl.Attach(b,uint32 (i+1),uint32 (i+2),uint32 j,uint32 (j+1))
                    else let l = new Label(value.ToString())                         
                         add l (i+1) j  
                         //tbl.Attach(l,uint32 (i+1),uint32 (i+2),uint32 j,uint32 (j+1))
                    ) ctx
                ) Game.contextsResponses       
            let vbox = new VBox()                
            let hbox = new HBox ()
            hbox.Add(lbl)
            let reinit = new Button("Randomize")
            reinit.Clicked.Add(fun _ -> Game.reinit()) 
            hbox.PackEnd(reinit)
            vbox.Add(hbox)           
            vbox.Add(tbl)
            win.Add(vbox)
            win.DeleteEvent.Add(fun _ -> ruleselector := null)
            ruleselector := win
            win.ShowAll()        

        member this.ShowRuleSelector() = 
            if !ruleselector = null
            then mkRuleSelector()
            else (!ruleselector).Present()
            

        member this.OnLoad () =
            use dlg = new Gtk.FileChooserDialog("load file",null,Gtk.FileChooserAction.Open,
                                                [| box "Cancel"; 
                                                   box ResponseType.Cancel; 
                                                   box "Open"; 
                                                   box ResponseType.Accept |]) in
            if dlg.Run() = int ResponseType.Accept 
            then Game.loadImage (dlg.Filename.ToString())
            dlg.Hide()

        member this.OnSave () =
            use dlg = new Gtk.FileChooserDialog("save file",null,Gtk.FileChooserAction.Save,
                                                [| box "Cancel"; 
                                                   box ResponseType.Cancel; 
                                                   box "Save"; 
                                                   box ResponseType.Accept |])
            if dlg.Run() = int ResponseType.Accept 
            then Game.saveImage (dlg.Filename.ToString())
            dlg.Hide()

        member this.ToggleThrottling () = throttling := not (!throttling)
                              
        member this.TogglePause () =
            paused := not (!paused)
            if not !paused then ignore (GLib.Idle.Add(idleHnd))
                              
        member this.OnDeleteEvent(o, e : DeleteEventArgs) = 
            Application.Quit()
            e.RetVal <- true

        member this.OnDaExposeEvent(o, e) =
            //See Gtk.dotnet.FromDrawable!!!
            let da = o :?> DrawingArea
            let sizex = ref 0 
            let sizey = ref 0             
            let win = da.GdkWindow
            win.GetSize(sizex,sizey)
            //let pb = new Gdk.Pixbuf(Gdk.Colorspace.Rgb,false,24,100,100)
            let (kx,ky) = (1.0 / (float Game.sizex),1.0 / (float Game.sizey))                    
            use cr = Gdk.CairoHelper.Create(win)
            cr.Scale(float (!sizex),float (!sizey))
            Game.blitImage 
                (fun x y (r,g,b) ->
                    cr.SetSourceRGB(float r / 255.0,float g / 255.0,float b / 255.0)
                    cr.Rectangle(kx * float x,ky * float y,2.0*kx,2.0*ky)
                    cr.Fill())
            