namespace DUE_FSharp_SPASandbox_2026

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.Sitelets

type EndPoint =
    | [<EndPoint "">] Home
    | [<EndPoint "echo">] Echo of string
    | [<EndPoint "form">] Form
    | [<EndPoint "forms">] Forms
    | [<EndPoint "charting">] Charting
    | [<EndPoint "maps">] Maps

[<JavaScript>]
module Client =
    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    let People =
        ListModel.FromSeq [
            "John"
            "Paul"
        ]

    // Create a router for our endpoints
    let router = Router.Infer<EndPoint>()
    // Install our client-side router and track the current page
    let currentPage = Router.InstallHash EndPoint.Home router

    module Pages =
        let Home () =
            async {
               return
                    div [attr.``class`` "p-6 bg-slate-100 min-h-screen"]
                    [
                        h1 [attr.``class`` "text-3xl font-bold mb-2 text-slate-800"]
                            [text "Senior Care Companion"]

                        p [attr.``class`` "text-slate-600 mb-6"]
                            [text "An elderly support web application for medication, appointments, family support and daily care."]

                        div [attr.``class`` "grid gap-4 md:grid-cols-3"]
                            [
                                div [attr.``class`` "bg-white p-5 rounded-2xl shadow"]
                                    [
                                        h2 [attr.``class`` "text-xl font-semibold mb-2"]
                                            [text "Today's Status"]
                                        p [attr.``class`` "text-green-700 font-medium"]
                                            [text "Everything is under control ✔"]
                                    ]

                                div [attr.``class`` "bg-white p-5 rounded-2xl shadow"]
                                    [
                                        h2 [attr.``class`` "text-xl font-semibold mb-2"]
                                            [text "Next Appointment"]
                                        p [attr.``class`` "text-slate-600"]
                                            [text "Doctor appointment at 14:00"]
                                    ]

                                div [attr.``class`` "bg-white p-5 rounded-2xl shadow"]
                                    [
                                        h2 [attr.``class`` "text-xl font-semibold mb-2"]
                                            [text "Medication"]
                                        p [attr.``class`` "text-slate-600"]
                                            [text "Morning medication: completed"]
                                        p [attr.``class`` "text-red-600 font-medium"]
                                            [text "Evening medication: pending"]
                                    ]
                            ]
                        div [attr.``class`` "mt-6 bg-white p-5 rounded-2xl shado"]
                            [
                                h2 [attr.``class`` "text-xl font-semibold mb-2"]
                                    [text "Planned Modules"]

                                ul [attr.``class`` "list-disc pl-6 text-slate-600 space-y-1"]
                                    [
                                        li [] [text "Medication checklist"]
                                        li [] [text "Medical appointments and senior programs"]
                                        li [] [text "Shopping list"]
                                        li [] [text "Important documents"]
                                        li [] [text "Family photo gallery"]
                                        li [] [text "Safety status: location, battery and last activity"]  
                                    ]
                            ]
                    ]
            }

        let Echo (msg:string) =
            text msg

        let FormPage () =
            IndexTemplate.Form()
                .OnSubmit(fun e ->
                    let v = e.Vars.Name.Value
                    JS.Alert <| sprintf "You typed: %s" v
                )
                .Doc()

        open WebSharper.Forms

        let Forms () =
            let res = Var.Create ""
            Form.Return (fun fn ln age ->
                { FirstName = fn; LastName = ln; Age = int age })
            <*> (Form.Yield "Your first name"
                |> Validation.IsNotEmpty "Please a name.")
            <*> (Form.Yield "Your last name"
                |> Validation.IsNotEmpty "Please a name.")
            <*> Form.Yield "0"
            |> Form.WithSubmit
            |> Form.Run (fun p ->
                async {
                    let! ret = Server.SavePerson p
                    match ret with
                    | Some p ->
                        res.Set <| sprintf "Saved %A!" p
                    | None ->
                        res.Set <| "Failure!"
                } |> Async.StartImmediate
            )
            |> Form.Render (fun fn ln age submitter ->
                IndexTemplate.PersonForm()
                    .FirstName(fn)
                    .LastName(ln)
                    .Age(age)
                    .OnSubmit(fun e ->
                        submitter.Trigger())
                    .Result(res.View)
                    .Doc()
            )

        open WebSharper.Charting

        let Charting() =
            let labels =
                [| "Eating"; "Drinking"; "Sleeping";
                   "Designing"; "Coding"; "Cycling"; "Running" |]
            let dataset1 = [|28.0; 48.0; 40.0; 19.0; 96.0; 27.0; 100.0|]
            let dataset2 = [|65.0; 59.0; 90.0; 81.0; 56.0; 55.0; 40.0|]
    
            let chart =
                Chart.Combine [
                    Chart.Radar(Array.zip labels dataset1)
                        .WithFillColor(Color.Rgba(151, 187, 205, 0.2))
                        .WithStrokeColor(Color.Name "blue")
                        .WithPointColor(Color.Name "darkblue")
                        .WithTitle("Alice")

                    Chart.Radar(Array.zip labels dataset2)
                        .WithFillColor(Color.Rgba(220, 220, 220, 0.2))
                        .WithStrokeColor(Color.Name "green")
                        .WithPointColor(Color.Name "darkgreen")
                        .WithTitle("Bob")
                ]
    
            Renderers.ChartJs.Render(chart, Size = Size(500, 300))

        open WebSharper.Leaflet
        
        let Maps() =
            let coordinates = div [] [] :?> Elt
            Leaflet.Styles.Style()
            div [] [
                div [
                    attr.style "height: 500px;"
                    on.afterRender (fun div ->
                        let map = Leaflet.L.Map(div)
                        map.SetView((47.49883, 19.0582), 14)
                        map.AddLayer(
                            Leaflet.TileLayer(
                                Leaflet.TileLayer.OpenStreetMap.UrlTemplate,
                                Leaflet.TileLayer.Options(
                                    Attribution = Leaflet.TileLayer.OpenStreetMap.Attribution)))
                        map.AddLayer(
                            let m = Leaflet.Marker((47.4952, 19.07114))
                            m.BindPopup("IntelliFactory")
                            m)
                        map.On_mousemove(fun map ev ->
                            coordinates.Text <- "Position: " + ev.Latlng.ToString())
                        map.On_mouseout(fun map ev ->
                            coordinates.Text <- "")
                    )
                ] []
                coordinates
            ]

    [<SPAEntryPoint>]
    let Main () =
        let newName = Var.Create ""

        let renderInnerPage (currentPage: Var<EndPoint>) =
            currentPage.View.Map (fun endpoint ->
                match endpoint with
                | Home ->
                    Pages.Home()
                    |> Doc.Async
                | Echo msg ->
                    Pages.Echo msg
                | Form ->
                    Pages.FormPage()
                | Forms ->
                    Pages.Forms()
                | Charting ->
                    Pages.Charting()
                | Maps ->
                    Pages.Maps()
            )
            |> Doc.EmbedView

        IndexTemplate()
            .Content(
                renderInnerPage currentPage
                //client (...)
                //hydrate (...)
            )
            //.ListContainer(
            //    People.View.DocSeqCached(fun (name: string) ->
            //        IndexTemplate.ListItem().Name(name).Doc()
            //    )
            //)
            //.Name(newName)
            //.Add(fun e ->
            //    People.Add(newName.Value)
            //    newName.Value <- ""
            //)
            .Bind()
