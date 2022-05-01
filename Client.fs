namespace calc2

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.UI.Templating

[<JavaScript>]
module Client =
    
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    [<SPAEntryPoint>]
    let Main () =
        let mutable (onum:double), (num:double), op = 0., 0., None

        let display = input [attr.``type`` "text"; attr.value "0"] []


        let updateDisplay () =

            let display = display :?> Elt


            display.Value <- string num

        let D n =
            num <- 10. * num + n
            updateDisplay ()

        let C () =
            num <- 0.
            updateDisplay()

        let AC () =
            num  <- 0.
            onum <- 0.
            op   <- None
            updateDisplay ()

        let OPP () =
            num <- - num
            updateDisplay ()

        let E () =
            match op with
            | None ->
                ()
            | Some f ->
                num <- f onum num
                op  <- None
                if (num = -infinity) || (num = infinity) then
                    let display = display :?> Elt
                    display.Value <- "E"
                    num <- 0.
                else
                    updateDisplay ()
        let DIV2 () =
            num <- 1. / num
            if (num = -infinity) || (num = infinity) then
                let display = display :?> Elt
                display.Value <- "E"
                num <- 0.
            else
                updateDisplay ()
        let COS () =
            num <- Math.Cos(num)
            updateDisplay ()

        let SIN () =
            num <- Math.Sin(num)
            updateDisplay ()
        let O o () =
            match op with
            | None ->
                ()
            | Some f ->
                num <- f onum num
                updateDisplay ()
            onum <- num
            num  <- 0.
            op   <- Some o

        let btn caption cl action =
            button [attr.``class`` cl; on.click (fun _ _ -> action ())] [text caption]

        let digit n =
            btn (string n) "D" (fun () -> D n)

        let calculator =
            div [] [
                display
                br [] []
                div [] [
                   
                    br [] []
                    digit    7.        ; digit     8.        ; digit    9.          ; btn "/" "DIV" (O ( / ))
                    br [] []
                    digit    4.        ; digit     5.        ; digit    6.          ; btn "*" "MULT" (O ( * ))
                    br [] []
                    digit    1.        ; digit     2.        ; digit    3.          ; btn "-" "SUBS" (O ( - ))
                    br [] []
                    digit    0.        ;  btn "AC"  "AC" AC  ;  btn "C"   "C"  C ; btn "+" "PLUS" (O ( + ))
                    br [] []
                    btn "Cos" "COS" COS ; btn "Sin" "SIN" SIN; btn "1/x" "DIV2"  DIV2 ;btn "+/-" "OPP"  OPP
                    br [] []
                    btn "=" "E"  E 
                ]
            ]
       
        IndexTemplate.Main()
            .Main(
                calculator
                )
            .Doc()
        |> Doc.RunById "main"
