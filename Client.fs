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

    type CareTask = {
        Id: int
        Title: string
        Time: string
        Category: string
        IsDone: bool
    }

    type TaskFilter =
        | AllTasks
        | PendingTasks
        | DoneTasks

    let careTasks =
        Var.Create [
            { Id = 1; Title = "Morning medication"; Time = "08:00"; Category = "Medication"; IsDone = true }
            { Id = 2; Title = "Blood pressure check"; Time = "10:00"; Category = "Health"; IsDone = false }
            { Id = 3; Title = "Doctor appointment"; Time = "14:00"; Category = "Appointment"; IsDone = false }
            { Id = 4; Title = "Call family member"; Time = "18:00"; Category = "Family"; IsDone = false }
        ]

    let newTitle = Var.Create ""
    let newTime = Var.Create ""
    let newCategory = Var.Create ""
    let selectedFilter = Var.Create AllTasks

    let addTask () =
        if newTitle.Value <> "" then
            let newTask =
                { Id = int System.DateTime.Now.Ticks
                  Title = newTitle.Value
                  Time = newTime.Value
                  Category = newCategory.Value
                  IsDone = false }

            careTasks.Set (careTasks.Value @ [ newTask ])

            newTitle.Value <- ""
            newTime.Value <- ""
            newCategory.Value <- ""
    let filterTasks filter tasks =
        match filter with
        | AllTasks -> tasks
        | PendingTasks -> tasks |> List.filter (fun task -> not task.IsDone)
        | DoneTasks -> tasks |> List.filter (fun task -> task.IsDone)

    let categoryClass category =
        match category with
        | "Medication" -> "bg-blue-100 text-blue-700"
        | "Health" -> "bg-purple-100 text-purple-700"
        | "Appointment" -> "bg-green-100 text-green-700"
        | "Family" -> "bg-pink-100 text-pink-700"
        | _ -> "bg-slate-100 text-slate-700"

    let taskCard task =
       div [
           attr.``class`` "bg-white p-5 rounded-2xl shadow-md border border-slate-100 cursor-pointer hover:shadow-x1 transition-all duration-200"
           on.click (fun _ _ ->
                let updated =
                    careTasks.Value
                    |> List.map (fun t ->
                        if t.Id = task.Id then
                            { t with IsDone = not t.IsDone }
                        else
                            t
                    )

                careTasks.Set updated
            )
       ]
            [
                div [attr.``class`` "flex justify-between items-start gap-3"]
                    [
                        div []
                            [
                                h3 [attr.``class`` "text-lg font-semibold text-slate-800 mb-1"]
                                    [text task.Title]

                                p [attr.``class`` "text-sm text-slate-500 mb-2"]
                                    [text ("Time: " + task.Time)]

                                span [
                                    attr.``class`` (
                                        "text-xs px-3 py-1 rounded-full font-medium " +
                                        categoryClass task.Category
                                    )
                                ] [
                                    text task.Category
                                ]
                            ]

                        div [attr.``class`` "flex flex-col items-end gap-2"]
                            [
                                if task.IsDone then
                                    span [attr.``class`` "text-xs bg-green-100 text-green-700 px-3 py-1 rounded-full font-medium"]
                                        [text "Done"]
                                else
                                    span [attr.``class`` "text-xs bg-orange-100 text-orange-700 px-3 py-1 rounded-full font-medium"]
                                        [text "Pending"]

                                button [
                                    attr.``class`` "text-xs bg-red-100 text-red-700 px-3 py-1 rounded-full font-medium hover:bg-red-200"
                                    on.click (fun _ ev ->
                                        ev.StopPropagation()

                                        let updated =
                                            careTasks.Value
                                            |> List.filter (fun t -> t.Id <> task.Id)

                                        careTasks.Set updated
                                    )
                                ] [
                                    text "Delete"
                                ]
                            ]
                    ]
            ]

    let taskList =
        View.Map2 (fun tasks filter ->
            let visibleTasks =
                filterTasks filter tasks

            visibleTasks
            |> List.map taskCard
            |> Doc.Concat
        ) careTasks.View selectedFilter.View
        |> Doc.EmbedView

    let summaryCards =
        Doc.BindView (fun tasks ->

            let allCount =
                tasks
                |> List.length

            let pendingCount =
                tasks
                |> List.filter (fun t -> not t.IsDone)
                |> List.length

            let doneCount =
                tasks
                |> List.filter (fun t -> t.IsDone)
                |> List.length

            div [attr.``class`` "grid grid-cols-1 md:grid-cols-3 gap-4 mb-6"]
                [
                div [attr.``class`` "bg-white p-4 rounded-2xl shadow text-center"] 
                    [
                        p [attr.``class`` "text-slate-500 text-sm"] 
                            [text "All tasks"]
                        h3 [attr.``class`` "text-3xl font-bold mt-2 text-slate-800"]
                            [text (string allCount)]
                    ]

                div [attr.``class`` "bg-white p-4 rounded-2xl shadow text-center"] 
                    [
                        p [attr.``class`` "text-slate-500 text-sm"] 
                            [text "Pending"]
                        h3 [attr.``class`` "text-3xl font-bold mt-2 text-orange-500"]
                            [text (string pendingCount)]
                    ]

                div [attr.``class`` "bg-white p-4 rounded-2xl shadow text-center"] 
                    [
                        p [attr.``class`` "text-slate-500 text-sm"] 
                            [text "Done"]
                        h3 [attr.``class`` "text-3xl font-bold mt-2 text-green-600"]
                            [text (string doneCount)]
                    ]
                ]
        ) careTasks.View

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
                        div [attr.``class`` "mt-6 bg-white p-5 rounded-2xl shadow"]
                            [
                                h2 [attr.``class`` "text-xl font-semibold mb-4"]
                                    [text "Add New Task"]

                                div [attr.``class`` "grid gap-3 md:grid-cols-4"]
                                    [
                                        Doc.Input [attr.placeholder "Task name"; attr.``class`` "border rounded-xl px-3 py-2"] newTitle

                                        Doc.Input [attr.placeholder "Time"; attr.``class`` "border rounded-xl px-3 py-2"] newTime

                                        select [
                                            attr.``class`` "border rounded-xl px-3 py-2 bg-white text-slate-700"
                                            on.change (fun el _ ->
                                                newCategory.Value <- el?value
                                            )
                                        ] [
                                            option [attr.value ""] [text "Select category"]
                                            option [attr.value "Medication"] [text "Medication"]
                                            option [attr.value "Health"] [text "Health"]
                                            option [attr.value "Appointment"] [text "Appointment"]
                                            option [attr.value "Family"] [text "Family"]
                                        ]

                                        button [
                                            attr.``class`` "bg-blue-600 text-white rounded-xl px-4 py-2"
                                            on.click (fun _ _ -> addTask())
                                        ] [text "Add"]
                                    ]
                            ]
                        div [attr.``class`` "mt-6"]
                            [
                                h2 [attr.``class`` "text-2xl font-bold mb-4 text-slate-800"]
                                    [text "Today's Care Tasks"]
                                
                                summaryCards


                                div [attr.``class`` "flex flex-wrap gap-3 mb-4"]
                                    [

                                        button [
                                            attr.``class`` "px-4 py-2 rounded-xl bg-blue-600 text-white"
                                            on.click (fun _ _ -> selectedFilter.Value <- AllTasks)
                                        ] [ text "All" ]

                                        button [
                                            attr.``class`` "px-4 py-2 rounded-xl bg-orange-500 text-white"
                                            on.click (fun _ _ -> selectedFilter.Value <- PendingTasks)
                                        ] [ text "Pending" ]

                                        button [
                                            attr.``class`` "px-4 py-2 rounded-xl bg-green-600 text-white"
                                            on.click (fun _ _ -> selectedFilter.Value <- DoneTasks)
                                        ] [ text "Done" ]

                                    ]

                                div [attr.``class`` "grid gap-4 md:grid-cols-2"]
                                    [
                                        taskList
                                    ]
                            ]
                        div [attr.``class`` "mt-6 bg-white p-5 rounded-2xl shadow"]
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
