// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace Sleeplog

open System
open System.Windows.Forms
open Components.Display
open Components.Data
open System.Drawing


module main =
    [<STAThread>]
    [<EntryPoint>]
    let main argv = 

        let today = System.DateTime.Today
        printfn "%A" (today.ToShortDateString())
        printfn "%A" (today.ToLongDateString())

        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault false
        
        let form = new Form(Width = 850, Height = 700, FormBorderStyle = FormBorderStyle.FixedSingle)
        form.AllowDrop <- true
        let np = new NodePanel(form.Height)
        let heading = new H1("Sleep Log")
        heading.Location <- new Point((form.Width / 2) - 40   , 20)

        
        let controls = form.Controls
        let console = new TextConsole(form.Width, form.Height - 200, form.Width, form.Height)
        //not working yet
        let day = new DayEntryForm()
        day.Location <- new Point (200,100)
        controls.Add console
        controls.Add np
        controls.Add heading
        controls.Add day


        console.write "TEST"
        Application.Run(form);

        0 // return an integer exit code
