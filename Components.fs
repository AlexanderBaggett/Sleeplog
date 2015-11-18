namespace Sleeplog

module Components =
    open System
    open System.Windows.Forms
    open System.Drawing
    open System.Threading.Tasks
    open System.Threading
    open System.Collections
    


     module Data =

        
        type Activity =
            | Activities of List<Activity>
            | Work
            | Exercise
            | Yoga
            | Stuff
            | Meditation
            | Gaming
            
        type Activities = 
             {  Work :bool ;
                Exercise : bool;
                Yoga: bool;
                Stuff: bool;
                Meditation : bool;
                Gaming : bool;
             }
             member this.getList() =
                 let list = new System.Collections.Generic.List<string>()
                 list.Add  "Work"
                 list.Add "Exercise"
                 list.Add "Yoga"
                 list.Add "Stuff"
                 list.Add "Meditation"
                 list.Add "Gaming"
                 list
            

        type SleepEvent =
            | Inbed of DateTime * List<SleepEvent> 
            | Leavebed of DateTime * Activity Option
            | Asleep of DateTime
            | Wakeup of DateTime

   

        type Day(Se :List<SleepEvent>, Date) =
         let x = 10
         let SleepEvents = Se

         member this.Date = Date
         member this.AddSleepEvent  (se : SleepEvent)= 
           let dot = SleepEvents @ [se]
           let newday = new Day(dot, this.Date)
           newday
         member this.GetDisplay =
            let treeview1 = new TreeNode()

            let matchChildNodes(se) : TreeNode =
                match se with
                | Asleep(time) -> let node = new TreeNode("Asleep " + time.ToShortTimeString());
                                  node
                | Wakeup(time) -> let node = new TreeNode("Wake up " + time.ToShortTimeString());
                                  node
                | _ -> failwith("Inbed can't have SubEvents other than Asleep and Wakeup")



            let simplify (node:TreeNode,hostNode:TreeNode, doer:(Activity List * TreeNode -> TreeNode), xs:List<Activity> ) =
                hostNode.Nodes.Add(node)  |> ignore
                doer(xs,hostNode)

            let rec matchActivityType(ae, hostNode:TreeNode) : TreeNode =
                    match ae with
                    | x :: xs -> match x with 
                                    | Work ->        let node = new TreeNode("Work")  
                                                     simplify(node,hostNode,matchActivityType,xs)

                                    | Exercise ->    let node = new TreeNode("Exercise")  
                                                     simplify(node,hostNode,matchActivityType,xs)

                                    | Yoga ->        let node = new TreeNode("Yoga")
                                                     simplify(node,hostNode,matchActivityType,xs)

                                    | Stuff ->       let node = new TreeNode("Stuff")
                                                     simplify(node,hostNode,matchActivityType,xs)

                                    | Meditation ->  let node = new TreeNode("Meditation")
                                                     simplify(node,hostNode,matchActivityType,xs)

                                    | Gaming ->      let node = new TreeNode("Gaming")
                                                     simplify(node,hostNode,matchActivityType,xs)
                                            
                                    | _ -> matchActivityType(xs, hostNode)
                    | [] ->  hostNode
            

            let rec buildChildSleepDisplay(se, hostNode:TreeNode) :TreeNode =
                match se with
                | x :: xs ->  let childNode  = matchChildNodes(x)
                              hostNode.Nodes.Add(childNode) |> ignore
                              buildChildSleepDisplay(se, hostNode)
                | [] -> hostNode


            let buildChildActivityDisplay ( ae, hostNode:TreeNode) :TreeNode =
                match ae with
                | Some (ae) ->  match ae with 
                                | Activities (aes) -> matchActivityType(aes, hostNode) |> ignore
                                                      hostNode
                                | other ->  matchActivityType([other], hostNode) |> ignore
                                            hostNode
                | None -> hostNode


            let simplifier2 (state :string, time :DateTime, xs: SleepEvent List , doer:(SleepEvent List -> unit ), children :bool, sleepevents : SleepEvent List Option, activity : Activity Option  ) =
 
                  if  not children then do
                     let node = new TreeNode(state + " " + time.ToShortTimeString())
                     treeview1.Nodes.Add(node) |> ignore
                    //build the rest of the list
                     doer(xs)
                  else do

                        match sleepevents with
                        | Some(x) ->  let node = new TreeNode(state + " " + time.ToShortTimeString())
                                      let node = buildChildSleepDisplay(x,node)
                                      treeview1.Nodes.Add(node) |> ignore
                                      //build the rest of the list
                                      doer(xs)

                         //must be an activity instead   
                        | None ->      
                                        let node = new TreeNode(state + " " + time.ToShortTimeString())
                                        let node = buildChildActivityDisplay(activity, node)
                                        treeview1.Nodes.Add(node) |> ignore
                                        doer(xs)       
                                                


            let rec buildDisplay (se:List<SleepEvent>) =

                match se with
                | x :: xs ->  match x with
                              | Inbed(time,sleepevents ) ->  simplifier2("In bed", time, xs, buildDisplay, true, Some(sleepevents), None)
                                                           

                              | Leavebed (time, activity) -> let node = new TreeNode("Leave bed" + time.ToShortTimeString())
                                                             let node = buildChildActivityDisplay(activity, node)
                                                             treeview1.Nodes.Add(node) |> ignore
                                                             //build the rest of the list
                                                             buildDisplay(xs)

                              | Asleep (time) -> simplifier2("Asleep", time, xs, buildDisplay, false, None, None) 


                              | Wakeup (time) ->  simplifier2("Wake up", time, xs, buildDisplay, false, None, None) 


                          
                | [] -> ()

            buildDisplay (SleepEvents)
            treeview1

         member this.HoursSlept = ()
         member this.AverageTimeToFallAsleep = ()
         new() = Day([], System.DateTime.Today.Date)

    module Display =
        open Data
      //  open Microsoft.FSharp.Core

        type Position =
        | Middle
        | Left
        | Right

        type InnerPanel() as this =
            inherit Panel()
            let pan1 =new Panel()
            let pan2 =new Panel()


            do
                pan1.BackColor <- Color.FromArgb(180,180,185)
                this.BackColor <- Color.FromArgb(200,200,200)
                pan1.Location <- new Point(15,15)
                pan1.Parent <- this
                pan1.AutoScroll <- true
                this.AllowDrop <- true
                pan2.BackColor <- Color.FromArgb(230,230,231)
                pan2.Location <- new Point(5,5)
                pan2.Parent <- pan1
                pan2.AutoScroll <- true
                this.AllowDrop <- true

                this.MouseDown.Add (fun _ ->
                                             //this.Location <- new Point( Cursor.Position.X - this.Width /2 , Cursor.Position.Y - this.Height / 2)
                                             this.DoDragDrop(this, DragDropEffects.Move)  |> ignore
                                             Console.WriteLine "mousedown innerpanel"
                                             )

            member this.Width 
                    with get() = base.Width
                    and  set (w) =
                        pan1.Width <- w - 30
                        pan2.Width <- w - 40
                        base.Width <- w

                         //location needs to be a function of the width/height
                        pan1.Location <- new Point(15,15)
                        pan2.Location <- new Point(5,5)
            member this.Height
                    with get() = base.Height
                    and  set (h) =
                        pan1.Height <- h - 30
                        pan2.Height <- h - 40
                        base.Height <- h

                        //location needs to be a function of the width/height
                        pan1.Location <- new Point(15,15)
                        pan2.Location <- new Point(5,5)
            member this.Children =
                pan2.Controls


        type TextConsole(w1,h1, w2,h2)  as this =
            inherit TextBox()
            do
                //let x = MessageBox.Show("constructor called") 
                this.Multiline <- true
               // this.Bounds <- new Rectangle(w1,h1,w2,h2)
                this.Width <- w1 - 20
                this.Height <- 100
                this.FontHeight <- 10
                this.Location <- new Point(2,h2 - 140)
                this.BackColor <- Color.FromArgb(55,55,55)
                this.ForeColor <- Color.FromArgb(150,180,220)
           
            member this.write text = 
                this.Text <- (this.Text + System.Environment.NewLine + text)

        type NodePanel(height) as this =
            inherit Panel()  

            do
                this.Height <-height - 170
                this.Location <- new Point (4,25)
                this.BackColor <- Color.White
                this.Width <- 150

        type H1(text, full) as this =
            inherit Label()
            do
                this.BackColor <- Color.FromArgb (210,210,210)
                this.Text <- text
                this.Font <- new Font("Arial", float32 20.0)
                this.Height <- 35
                this.Width <- text.Length * 16
                if this.fullwidth then this.Width <- 20000

            member this.fullwidth = full
            new (text) = new H1(text,false)
               
        type H2(text) as this =
            inherit H1(text)
            do
               this.BackColor <- Color.FromArgb (220,220,220)
               this.Font <- new Font("Arial", float32 15.0)
               this.Height <- 30



          type SleepEventEntry(lastevent :string) as this =
            inherit InnerPanel()

            let baseWidth = 90
           
        
            let mutable types = [|"In bed --0"; "Leave bed --1" ; "Asleep --2" ; "Wake up --3"|]
            let eventTypeDropdown =  new ComboBox(Width = baseWidth,  Location = new Point(130, 66))
            let timeStamp = new TextBox(Width = baseWidth,  Location = new Point (130, 108))



            do
                Parallel.Invoke(
                                 (fun _ ->
                                                  match lastevent with
                                                  | "In bed --1" ->  types <-   [|"Leave bed --1" ; "Asleep --2"|]          
                                                  | "" -> ()
                                                  | "Leave bed --1" -> types <- [|"In bed --0"; "Asleep --2" ; "Wake up --3"|]
                                                  | "Asleep --2" ->   types <-  [|"Wake up --3"|]
                                                  | "Wake up --3" ->  types <-  [|"Leave bed --1" ; "Asleep --2" ; "Wake up --3"|]
                                                  | _ -> failwith "unknown last sleep event type"

                                                  //let eventTypeDropdown =  new ComboBox(                            Width = baseWidth,  Location = new Point(130, 66))
                                                  let eventLabel = new Label(Text = "Sleep Event Type",                                 Location = new Point(20,70))
                                                  let timeLabel = new Label(Text = "Enter Time",                    Width = baseWidth,  Location = new Point (20,110))      
                                                  this.Children.Add eventTypeDropdown 
                                                  this.Children.Add eventLabel
                                                  this.Children.Add timeLabel
                                                  eventTypeDropdown.DropDownStyle <- ComboBoxStyle.DropDownList
                                                  
                                                  types
                                                  |> Array.map (fun t -> (upcast t): Object)
                                                  |> eventTypeDropdown.Items.AddRange
                                                  eventTypeDropdown.SelectedIndex <- 0

                                                  
                                                   )//end of init1    
                                ,(fun _ -> 
                                

                                                  let mainLabel = new Label(Text = "Add Sleep Event", Height = 50 , Width = 190, Location = new Point(30,10), Font = new Font("Arial", float32 16.0), ForeColor = Color.FromArgb(150,200,210))
                                                  this.Children.Add mainLabel
                                                  this.Children.Add timeStamp

                                                   )//end of init2  


                                ,(fun _ ->         
                                
                                            let addButton = new Button(Text = "Add Event",                    Width = baseWidth,  Location = new Point (20,150))
                                            let cancelButton = new Button(Text = "Cancel",                    Width = baseWidth,  Location = new Point (130, 150))


                                            this.Children.Add addButton
                                            this.Children.Add cancelButton

                                            let remove _  =
                                                                             let parentControls = this.Parent.Controls 
                                                                             parentControls.Remove this
                                                                             this.Hide()
                                                                             this.Dispose()
                                            cancelButton.Click.Add (remove)
                                            addButton.Click.Add ( fun _ -> 
                                                                             let time = DateTime.Parse(timeStamp.Text)

                                                                             match (eventTypeDropdown.SelectedItem.ToString())  with
                                                                             |"In bed --0" -> let x =  SleepEvent.Inbed(time,[]); 
                                                                                              this.SleepEvent <- x
                                                                             |"Leave bed --1" -> let x =  SleepEvent.Leavebed(time, None)
                                                                                                 this.SleepEvent <- x
                                                                             |"Asleep --2" -> let x =  SleepEvent.Asleep(time)
                                                                                              this.SleepEvent <- x
                                                                             | "Wake up --3" ->   let x =  SleepEvent.Wakeup(time)
                                                                                                  this.SleepEvent <- x
                                                                             | x ->  MessageBox.Show (x) |> ignore
                                                                             | _ -> failwith("Unknown type")
                                                                             remove ""
                                                                                      )
                                                                                                

                                                   )//end of init3  

                                )//end of parallel.invoke


                
                this.Width <- 270
                this.Height <- 215
               
                Console.WriteLine( "")
            member val SleepEvent :SleepEvent = SleepEvent.Asleep(DateTime.Now) with get, set
            new() = new SleepEventEntry("")



            type DayEntryForm() as this =
                inherit InnerPanel()
                let baseWidth = 90
                let column1 = 20
                let column2 = 140
                let addDayLabel = new Label(Text = "Add Day", Width = baseWidth * (6 / 3), Location = new Point(65,10), Font = new Font("Arial", float32 16.0), ForeColor = Color.FromArgb(150,200,210))
                let addsleepEventButton = new Button(Text = "Add Sleep Event" ,Width = baseWidth , Location = new Point (column1,50) )
                let addActivityButton = new Button(Text = "Add Activity", Width = baseWidth, Location = new Point (column2,50) )
                let todayTextBox = new TextBox(Text = "", Width = baseWidth, Location = new Point(column1,100))
                let todayButton = new Button(Text = "Today", Width = baseWidth, Location = new Point(column2,100))
                let displayLabel = new Label(Text = "Current Children", Location = new Point (column1, 130 ), Width = baseWidth * (6 / 3), Font = new Font("Arial", float32 16.0), ForeColor = Color.FromArgb(150,200,210))
                let displayList = new Panel(BackColor = Color.White, Location = new Point (column1,160), Height = 140, Width = 210, AutoScroll = true)
                let checkbox =  new CheckedListBox( Location = new Point(column1, 300))
                let testact =          {    Work = true ;
                                            Exercise = true;
                                            Yoga= true;
                                            Stuff= true;
                                            Meditation = true;
                                            Gaming = true;}
             

                do
                //build the form
                    this.Children.Add addDayLabel
                    this.Children.Add addsleepEventButton
                    this.Children.Add addActivityButton
                    this.Children.Add todayTextBox
                    this.Children.Add todayButton
                    this.Children.Add displayList
                    this.Children.Add displayLabel
                    this.Children.Add checkbox
                    this.Height <- 400
                    this.Width <- 600
                    todayButton.Click.Add (fun _ -> todayTextBox.Text <- System.DateTime.Today.ToShortDateString() )
                    addsleepEventButton.Click.Add (fun _ ->
                                                            let sleepPanel = new SleepEventEntry()
                                                            sleepPanel.Location <- new Point(290,20)
                                                            this.Children.Add sleepPanel
                                                            sleepPanel.Show()
                                                             )
                    displayList.Paint.Add (
                                            fun sender -> 
                                                         let baserect = sender.ClipRectangle
                                                         let rect = new Rectangle(baserect.X, baserect.Y, baserect.Width , baserect.Height)
                                                         ControlPaint.DrawBorder(sender.Graphics, rect, Color.Black, ButtonBorderStyle.Solid)
                                            
                                            
                                            )
                    displayList.AutoScroll <- true
                    
                    checkbox.DataSource <- testact.getList()
                    

                member val SleepEvents : SleepEvent List = [] 
                    with get, set
                member this.addSleepEvent se =
                 // easy part is the data
                  this.SleepEvents <- (se :: this.SleepEvents)

                  //hard part is the GUI
                  let simplifier (time:DateTime, typelabel, ypos, parent:Control) =
                      let label = new Label(Text = typelabel + " " + time.TimeOfDay.ToString(), Location = new Point(0, ypos)  );
                      parent.Controls.Add label

                  let simplifier2 (time:DateTime, typelabel, ypos, parent:Control, callback, list) =
                      let label = new Label(Text = typelabel + " " + time.TimeOfDay.ToString(), Location = new Point(0, ypos)  );
                      parent.Controls.Add label
                      callback(list, ypos)
                  

                  let rec doer (list: SleepEvent List, ypos) =
                          match list with
                          | x :: xs ->    match x with 
                                          | Inbed (time, sleepevents) ->  simplifier(time, "In bed", ypos, displayList)
                                                                          match sleepevents with
                                                                          | x :: xs ->   doer(sleepevents , ypos + 20)
                                                                          | [] ->  doer( xs , ypos + 20)
                                                                    
                                                                          
                                          | Leavebed (time, activities) -> simplifier2(time, "Leave Bed", ypos, displayList, doer, list)
                                              
                                          | Asleep (time) -> simplifier2(time, "Asleep", ypos, displayList, doer, list)
                                          | Wakeup  (time) -> simplifier2(time, "Wakeup", ypos, displayList, doer, list)

                          | [] -> ()

                  doer (this.SleepEvents, 0)

                member this.RemoveSleepEvent se = ()
               