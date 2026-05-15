namespace DUE_FSharp_SPASandbox_2026

open WebSharper
open WebSharper.JavaScript
open WebSharper.JavaScript.Dom
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

    type GalleryAlbum =
        | AllPhotos
        | FamilyPhotos
        | GardenPhotos
        | BirthdayPhotos

    type GalleryPhoto = {
        Id: int
        Title: string
        Album: string
        Icon: string
        ImageData: string
        UploadedAt: string
    }

    type AppPage =
        | DashboardPage
        | TasksPage
        | GalleryPage
        | ProfilePage
        | SettingsPage

    let careTasks : Var<list<CareTask>> =
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
    let selectedGalleryAlbum = Var.Create AllPhotos
    let galleryPhotos : Var<list<GalleryPhoto>> = Var.Create []
    let newPhotoTitle = Var.Create ""
    let newPhotoAlbum = Var.Create ""
    let newPhotoImageData = Var.Create ""
    let selectedFullscreenPhoto : Var<option<GalleryPhoto>> =
        Var.Create None
    let galleryStorageKey = "galleryPhotos"
    let selectedAppPage = Var.Create DashboardPage
    let mobileMenuOpen = Var.Create false
    let tasksStorageKey = "careTasks"

    let largeTextMode = Var.Create false
    let highContrastMode = Var.Create false
    let showReminders = Var.Create true
    let settingsStorageKey = "careSettings"

    let profileName = Var.Create ""
    let profileBirthDate = Var.Create ""
    let profileHeight = Var.Create ""
    let profileWeight = Var.Create ""
    let emergencyContact = Var.Create ""
    let emergencyPhone = Var.Create ""
    let medicalNotes = Var.Create ""
    let morningMedicationDone = Var.Create true
    let noonMedicationDone = Var.Create false
    let eveningMedicationDone = Var.Create false

    let saveSettings () =
        let savedText =
            string largeTextMode.Value + "§" +
            string highContrastMode.Value + "§" +
            string showReminders.Value

        JS.Window.LocalStorage.SetItem(settingsStorageKey, savedText)

    let loadSettings () =
        let savedText =
            JS.Window.LocalStorage.GetItem(settingsStorageKey)

        if savedText <> null && savedText <> "" then
            let parts = savedText.Split('§')

            if parts.Length = 3 then
                match System.Boolean.TryParse(parts.[0]) with
                | true, value -> largeTextMode.Value <- value
                | _ -> ()

                match System.Boolean.TryParse(parts.[1]) with
                | true, value -> highContrastMode.Value <- value
                | _ -> ()

                match System.Boolean.TryParse(parts.[2]) with
                | true, value -> showReminders.Value <- value
                | _ -> ()

    let saveProfile () =
        JS.Window.LocalStorage.SetItem("profileName", profileName.Value)
        JS.Window.LocalStorage.SetItem("profileBirthDate", profileBirthDate.Value)
        JS.Window.LocalStorage.SetItem("profileHeight", profileHeight.Value)
        JS.Window.LocalStorage.SetItem("profileWeight", profileWeight.Value)
        JS.Window.LocalStorage.SetItem("emergencyContact", emergencyContact.Value)
        JS.Window.LocalStorage.SetItem("emergencyPhone", emergencyPhone.Value)
        JS.Window.LocalStorage.SetItem("medicalNotes", medicalNotes.Value)

        JS.Alert "Profile saved successfully."

    let loadProfile () =
        let loadValue key (target: Var<string>) =
            let savedValue = JS.Window.LocalStorage.GetItem(key)

            if savedValue <> null then
                target.Value <- savedValue

        loadValue "profileName" profileName
        loadValue "profileBirthDate" profileBirthDate
        loadValue "profileHeight" profileHeight
        loadValue "profileWeight" profileWeight
        loadValue "emergencyContact" emergencyContact
        loadValue "emergencyPhone" emergencyPhone
        loadValue "medicalNotes" medicalNotes

    let taskToStorageLine (task: CareTask) =
        string task.Id + "§" +
        task.Title + "§" +
        task.Time + "§" +
        task.Category + "§" +
        string task.IsDone

    let taskFromStorageLine (line: string) =
        let parts = line.Split('§')

        if parts.Length = 5 then
            match System.Int32.TryParse(parts.[0]), System.Boolean.TryParse(parts.[4]) with
            | (true, id), (true, isDone) ->
                Some {
                    Id = id
                    Title = parts.[1]
                    Time = parts.[2]
                    Category = parts.[3]
                    IsDone = isDone
                }
            | _ ->
                None

        else
            None

    let saveTasks () =
        let savedText =
            careTasks.Value
            |> List.map taskToStorageLine
            |> String.concat "\n"

        JS.Window.LocalStorage.SetItem(tasksStorageKey, savedText)

    let loadTasks () =
        let savedText =
            JS.Window.LocalStorage.GetItem(tasksStorageKey)

        if savedText <> null && savedText <> "" then
            let loadedTasks =
                savedText.Split('\n')
                |> Array.choose taskFromStorageLine
                |> Array.toList

            if loadedTasks.Length > 0 then
                careTasks.Value <- loadedTasks

    let photoToStorageLine (photo: GalleryPhoto) =
        string photo.Id + "§" +
        photo.Title + "§" +
        photo.Album + "§" +
        photo.ImageData + "§" +
        photo.UploadedAt

    let photoFromStorageLine (line: string) =
        let parts = line.Split('§')

        if parts.Length = 5 then

            match System.Int32.TryParse(parts.[0]) with
            | true, id ->

                Some {
                    Id = id
                    Title = parts.[1]
                    Album = parts.[2]
                    Icon = "🖼️"
                    ImageData = parts.[3]
                    UploadedAt = parts.[4]
                }

            | _ ->
                None

        else
            None

    let saveGalleryPhotos () =

        let savedText =

            galleryPhotos.Value
            |> List.map photoToStorageLine
            |> String.concat "\n"

        JS.Window.LocalStorage.SetItem(galleryStorageKey, savedText)

    let loadGalleryPhotos () =

        let savedText =
            JS.Window.LocalStorage.GetItem(galleryStorageKey)

        if savedText <> null && savedText <> "" then

            let loadedPhotos =

                savedText.Split('\n')
                |> Array.choose photoFromStorageLine
                |> Array.toList

            galleryPhotos.Value <- loadedPhotos

    let albumPlaceholder album =
        match album with
        | "Family" -> "👨‍👩‍👧"
        | "Garden" -> "🌿"
        | "Birthday" -> "🎂"
        | _ -> "📷"

    let deleteGalleryPhoto photoId =
        let updatedPhotos =
            galleryPhotos.Value
            |> List.filter (fun photo -> photo.Id <> photoId)

        galleryPhotos.Value <- updatedPhotos
        saveGalleryPhotos()

    let addGalleryPhoto () =
        if newPhotoTitle.Value.Trim() <> "" && newPhotoAlbum.Value <> "" then

            let imageData =
                if newPhotoImageData.Value <> "" then
                    newPhotoImageData.Value
                else
                    albumPlaceholder newPhotoAlbum.Value

            let newPhoto =
                {
                    Id = int System.DateTime.Now.Ticks
                    Title = newPhotoTitle.Value
                    Album = newPhotoAlbum.Value
                    Icon = "🖼️"
                    ImageData = imageData
                    UploadedAt = System.DateTime.Now.ToString("yyyy-MM-dd HH:mm")
                }

            galleryPhotos.Value <- galleryPhotos.Value @ [newPhoto]
            saveGalleryPhotos()

            newPhotoTitle.Value <- ""
            newPhotoImageData.Value <- ""

    let addTask () =
        if newTitle.Value <> "" && newTime.Value <> "" && newCategory.Value <> "" then
            let newTask =
                { Id = int System.DateTime.Now.Ticks
                  Title = newTitle.Value
                  Time = newTime.Value
                  Category = newCategory.Value
                  IsDone = false }

            careTasks.Set (careTasks.Value @ [ newTask ])
            saveTasks()

            newTitle.Value <- ""
            newTime.Value <- ""
            newCategory.Value <- ""
    let filterTasks filter tasks =
        match filter with
        | AllTasks -> tasks
        | PendingTasks -> tasks |> List.filter (fun task -> not task.IsDone)
        | DoneTasks -> tasks |> List.filter (fun task -> task.IsDone)

    let navButtonClass selectedPage page=
        if selectedPage = page then
            "text-blue-700 bg-blue-50 px-3 py-2 rounded-xl font-semibold"
        else
            "text-slate-700 hover:text-blue-600 px-3 py-2 rounded-xl font-medium"

    let settingToggle title description (settingVar: Var<bool>) =
        Doc.BindView (fun isEnabled ->

            div [attr.``class`` "bg-white rounded-2xl shadow p-5 border border-slate-100 flex items-center justify-between gap-4"]
                [
                    div []
                        [
                            h3 [attr.``class`` "text-lg font-semibold text-slate-800"]
                                [text title]

                            p [attr.``class`` "text-sm text-slate-500 mt-1"]
                                [text description]
                        ]

                    button [
                        attr.``class`` (
                            if settingVar.Value then
                                "px-4 py-2 rounded-xl bg-blue-600 text-white font-medium shadow"
                            else
                                "px-4 py-2 rounded-xl bg-slate-200 text-slate-700 font-medium"
                        )

                        on.click (fun _ _ ->
                            settingVar.Value <- not settingVar.Value
                            saveSettings()
                        )
                    ] [
                        if settingVar.Value then
                            text "On"
                        else
                            text "Off"
                    ]
                ]
            ) settingVar.View

    let categoryClass category =
        match category with
        | "Medication" -> "bg-blue-100 text-blue-700"
        | "Health" -> "bg-purple-100 text-purple-700"
        | "Appointment" -> "bg-green-100 text-green-700"
        | "Family" -> "bg-pink-100 text-pink-700"
        | _ -> "bg-slate-100 text-slate-700"

    let medicationReminderCard title time (statusVar: Var<bool>) =
        Doc.BindView (fun isDone ->
            div [attr.``class`` "bg-white rounded-3xl shadow-lg p-6 border border-slate-100 hover:shadow-xl transition-all duration-300"]
                [
                    div [attr.``class`` "flex justify-between items-start"]
                        [
                            div []
                                [
                                    div [attr.``class`` "text-3xl"]
                                        [
                                            text "💊"
                                        ]

                                    div []
                                        [
                                            h3 [attr.``class`` "text-lg font-semibold text-slate-800"]
                                                [text title]

                                            p [attr.``class`` "text-sm text-slate-500 mt-1"]
                                                [text ("Scheduled time: " + time)]
                                        ]
                                ]

                            span [
                                attr.``class`` (
                                    if isDone then
                                        "text-xs bg-green-100 text-green-700 px-4 py-2 rounded-full font-semibold shadow-sm"
                                    else
                                        "text-xs bg-orange-100 text-orange-700 px-4 py-2 rounded-full font-semibold shadow-sm"
                                )
                            ] [
                                text (
                                    if isDone then
                                        "Completed"
                                    else
                                        "Pending"
                                )
                            ]
                        ]

                    button [
                        attr.``class`` (
                            if isDone then
                                "mt-4 w-full bg-slate-200 text-slate-700 rounded-xl px-4 py-3 font-medium"
                            else
                                "mt-4 w-full bg-blue-600 hover:bg-blue-700 text-white rounded-xl px-4 py-3 font-medium transition-all"
                        )

                        on.click (fun _ _ ->
                            statusVar.Value <- not statusVar.Value
                        )
                    ] [
                        text (
                            if isDone then
                                "Set as Pending"
                            else
                                title + " taken"
                        )
                    ]
                ]
        ) statusVar.View


    let taskCard (task: CareTask) =
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
                saveTasks()
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
                                        saveTasks()
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

            if List.isEmpty visibleTasks then

                div [attr.``class`` "bg-white rounded-2xl shadow p-10 text-center col-span-full"]
                    [
                        div [attr.``class`` "text-5xl mb-4"]
                            [
                                text "📋"
                            ]

                        h3 [attr.``class`` "text-xl font-semibold text-slate-700 mb-2"]
                            [
                                text "No tasks available"
                            ]

                        p [attr.``class`` "text-slate-500"]
                            [
                                text "Add a new care task to get started."
                            ]
                    ]
            else

                div [attr.``class`` "grid gap-4 md:grid-cols-2"]
                    [
                        yield!
                            visibleTasks
                            |> List.map taskCard
                    ]    

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
                div [attr.``class`` "bg-white p-5 rounded-2xl shadow text-center border border-slate-100"] 
                    [
                        div [attr.``class`` "text-4xl mb-3"]
                            [
                                text "📋"
                            ]
                        p [attr.``class`` "text-slate-500 text-sm font-medium"] 
                            [text "All tasks"]

                        h3 [attr.``class`` "text-3xl font-bold mt-2 text-slate-800"]
                            [text (string allCount)]
                    ]

                div [attr.``class`` "bg-white p-5 rounded-2xl shadow text-center border border-slate-100"] 
                    [
                        div [attr.``class`` "text-4xl mb-3"]
                            [
                                text "⏳"
                            ]

                        p [attr.``class`` "text-slate-500 text-sm font-medium"] 
                            [text "Pending"]
                        
                        h3 [attr.``class`` "text-3xl font-bold mt-2 text-orange-500"]
                            [text (string pendingCount)]
                    ]

                div [attr.``class`` "bg-white p-5 rounded-2xl shadow text-center border border-slate-100"] 
                    [
                        div [attr.``class`` "text-4xl mb-3"]
                            [
                                text "✅"
                            ]

                        p [attr.``class`` "text-slate-500 text-sm font-medium"] 
                            [text "Done"]
                        
                        h3 [attr.``class`` "text-3xl font-bold mt-2 text-green-600"]
                            [text (string doneCount)]
                    ]
                ]
        ) careTasks.View

    let renderAppPage page =
        match page with
        | DashboardPage -> 
            div [] [
                h1 [attr.``class`` "text-3xl font-bold mb-2 text-slate-800"]
                    [text "Today's Overview"]
            (*
                p [attr.``class`` "text-slate-600 mb-6"]
                    [text "Dashboard Page"]
*)
                div [attr.``class`` "grid gap-4 md:grid-cols-4"]
                    [
                        div [attr.``class`` "bg-white p-6 rounded-2xl shadow border border-slate-100 hover:shadow-xl hover:-translate-y-1 transition-all duration-300"]
                            [
                                div [attr.``class`` "flex items-center gap-3 mb-4"]
                                    [
                                        div [attr.``class`` "text-4xl"] 
                                            [
                                                text "✅"
                                            ]
                                            
                                        h2 [attr.``class`` "text-xl font-semibold text-slate-800"]
                                            [text "Today's Status"]
                                    ]

                                p [attr.``class`` "text-green-700 font-medium text-lg"]
                                    [text "Everything is under control ✔"]
                            ]
                        
                        div [attr.``class`` "bg-white p-6 rounded-2xl shadow border border-slate-100 hover:shadow-xl hover:-translate-y-1 transition-all duration-300"]
                            [
                                div [attr.``class`` "flex items-center gap-3 mb-4"]
                                    [
                                        div [attr.``class`` "text-4xl"]
                                            [
                                                text "📅"
                                            ]

                                        h2 [attr.``class`` "text-xl font-semibold text-slate-800"]
                                            [text "Next Appointment"]
                                    ]

                                p [attr.``class`` "text-slate-700 font-medium text-lg"]
                                    [text "Doctor appointment at 14:00"]
                            ]

                        div [attr.``class`` "bg-white p-6 rounded-2xl shadow border border-slate-100 hover:shadow-xl hover:-translate-y-1 transition-all duration-300"]
                            [
                                div [attr.``class`` "flex items-center gap-3 mb-4"]
                                    [
                                        div [attr.``class`` "text-4xl"]
                                            [
                                                text "💊"
                                            ]

                                        h2 [attr.``class`` "text-xl font-semibold text-slate-800"]
                                            [text "Medication"]
                                    ]
                                p [attr.``class`` "text-slate-700 font-medium text-lg"]
                                    [text "Morning medication: completed"]

                                p [attr.``class`` "text-red-600 font-medium mt-2"]
                                    [text "Evening medication: pending"]
                            ]

                    ]
                div [attr.``class`` "mt-6"]
                    [
                        summaryCards
                    ]

                div [attr.``class`` "mt-8 bg-white rounded-3xl shadow-lg p-6 border border-slate-100"]
                    [
                        h2 [attr.``class`` "text-2xl font-bold text-slate-800 mb-4"]
                            [
                                text "Family Updates"
                            ]

                        Doc.BindView (fun photos ->

                            if List.isEmpty photos then

                                div [attr.``class`` "text-slate-500"]
                                    [
                                        text "No family updates available."
                                    ]

                            else

                                let latestPhoto =
                                    photos |> List.last

                                div [attr.``class`` "space-y-3"]
                                    [
                                        div [attr.``class`` "flex items-center gap-3"]
                                            [
                                                div [attr.``class`` "text-3xl"]
                                                    [
                                                        text "📸"
                                                    ]

                                                div []
                                                    [
                                                        p [attr.``class`` "font-semibold text-slate-800"]
                                                            [
                                                                text ("New photo added to " + latestPhoto.Album + " album")
                                                            ]

                                                        p [attr.``class`` "text-sm text-slate-500"]
                                                            [
                                                                text latestPhoto.UploadedAt
                                                            ]
                                                    ]
                                            ]

                                        div [attr.``class`` "flex items-center gap-3"]
                                            [
                                                div [attr.``class`` "text-3xl"]
                                                    [
                                                        text "🖼️"
                                                    ]

                                                p [attr.``class`` "text-slate-700"]
                                                    [
                                                        text ("Total uploaded photos: " + string photos.Length)
                                                    ]
                                            ]
                                    ]

                        ) galleryPhotos.View
                    ]

                Doc.BindView (fun remindersEnabled ->

                    if remindersEnabled then

                        div [attr.``class`` "mt-8"]
                            [
                                h2 [attr.``class`` "text-2xl font-bold text-slate-800 mb-4"]
                                    [text "Medication Reminders"]

                                div [attr.``class`` "grid gap-4 md:grid-cols-3"]
                                    [
                                        medicationReminderCard
                                            "Morning medication"
                                            "08:00"
                                            morningMedicationDone

                                        medicationReminderCard
                                            "Noon medication"
                                            "12:00"
                                            noonMedicationDone

                                        medicationReminderCard
                                            "Evening medication"
                                            "18:00"
                                            eveningMedicationDone
                                    ]
                            ]
                    else
                        Doc.Empty

                ) showReminders.View

            ]

        | TasksPage ->
            div [] [
                h1 [attr.``class`` "text-3xl font-bold mb-2 text-slate-800"]
                    [text "Care Tasks"]
            
                div [attr.``class`` "bg-white p-5 rounded-2xl shadow"]
                    [
                        h2 [attr.``class`` "text-xl font-semibold mb-4"]
                            [text "Add New Task"]

                        div [attr.``class`` "grid gap-3 md:grid-cols-4"]
                            [    
                                Doc.Input [
                                    attr.placeholder "Task name"
                                    attr.``class`` "border border-slate-200 rounded-xl px-4 py-3 shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-400"
                                ] newTitle

                                select [
                                    attr.``class`` "border border-slate-200 rounded-xl px-4 py-3 bg-white text-slate-700 shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-400"
                                    on.change (fun el _ ->
                                        newTime.Value <- el?value
                                    )
                                ] [
                                    option [
                                        attr.value ""
                                        attr.selected "selected"
                                        attr.disabled "disabled"
                                    ] [
                                        text "Select time"
                                    ]

                                    option [attr.value "08:00"] [text "08:00"]
                                    option [attr.value "10:00"] [text "10:00"]
                                    option [attr.value "14:00"] [text "14:00"]
                                    option [attr.value "18:00"] [text "18:00"]
                                ]

                                select [
                                    attr.``class`` "border border-slate-200 rounded-xl px-4 py-3 bg-white text-slate-700 shadow-sm focus:ring-2 focus:ring-blue-400"
                                    on.change (fun el _ ->
                                        newCategory.Value <- el?value
                                    )
                                ] [

                                    option [
                                        attr.value ""
                                        attr.selected "selected"
                                        attr.disabled "disabled"
                                    ] [
                                        text "Select category"
                                    ]

                                    option [attr.value "Medication"] [text "Medication"]
                                    option [attr.value "Health"] [text "Health"]
                                    option [attr.value "Appointment"] [text "Appointment"]
                                    option [attr.value "Family"] [text "Family"]
                                ]

                                button [
                                    attr.``class`` "bg-blue-600 hover:bg-blue-700 transition-all text-white rounded-xl px-5 py-3 font-medium shadow-md"
                                    on.click (fun _ _ -> addTask())
                                ] [
                                    text "Add"
                                ]
                            ]
                    ]

                div [attr.``class`` "mt-6"]
                    [
                        div [attr.``class`` "flex flex-wrap gap-3 mb-4"]
                            [
                                button [
                                    attr.``class`` "px-4 py-2 rounded-xl bg-blue-600 text-white"
                                    on.click (fun _ _ ->
                                        selectedFilter.Value <- AllTasks
                                    )
                                ] [
                                    text "All"
                                ]

                                button [
                                    attr.``class`` "px-4 py-2 rounded-xl bg-orange-500 text-white"
                                    on.click (fun _ _ ->
                                        selectedFilter.Value <- PendingTasks
                                    )
                                ] [
                                    text "Pending"
                                ]

                                button [
                                    attr.``class`` "px-4 py-2 rounded-xl bg-green-600 text-white"
                                    on.click (fun _ _ ->
                                        selectedFilter.Value <- DoneTasks
                                    )
                                ] [
                                    text "Done"
                                ]
                            ]

                        div []
                            [
                                taskList
                            ]
                    ]
            ]
        | GalleryPage ->
            div [] [
                h1 [attr.``class`` "text-3xl font-bold mb-2 text-slate-800"]
                    [text "Family Gallery"]
            
                p [attr.``class`` "text-slate-600 mb-6"]
                    [text "Shared family memories and important photo updates"]

                div [attr.``class`` "bg-white rounded-3xl shadow-lg p-6 mb-6 border border-slate-100"]

                    [

                        h2 [attr.``class`` "text-2xl font-semibold text-slate-800 mb-4"]
                            [
                                text "Upload New Photo"
                            ]

                        div [attr.``class`` "grid gap-4 md:grid-cols-3"]

                            [

                                Doc.Input [

                                    attr.placeholder "Photo title"
                                    attr.``class`` "border border-slate-200 rounded-xl px-4 py-3"

                                ] newPhotoTitle


                                select [

                                    attr.``class`` "border border-slate-200 rounded-xl px-4 py-3 bg-white"

                                    attr.value newPhotoAlbum.Value

                                    on.change (fun el _ ->
                                        newPhotoAlbum.Value <- el?value
                                    )

                                ] [
                                    option [
                                        attr.value ""
                                        attr.selected "selected"
                                        attr.disabled "disabled"
                                    ] [
                                        text "Select album"
                                    ]

                                    option [attr.value "Family"] [text "Family"]
                                    option [attr.value "Garden"] [text "Garden"]
                                    option [attr.value "Birthday"] [text "Birthday"]

                                ]


                                label [

                                    attr.``class`` "bg-blue-600 hover:bg-blue-700 text-white rounded-xl px-5 py-3 font-medium shadow-md text-center cursor-pointer"

                                ]

                                    [

                                        text "Choose Image"

                                        input [

                                            attr.``type`` "file"
                                            attr.accept "image/*"
                                            attr.``class`` "hidden"

                                            on.change (fun el _ -> 
                                            
                                                let files = el?files
                                                if files?length > 0 then
                                                    let file = files?item(0)
                                                    let reader = JS.Eval("new FileReader()")
                                                    reader?onload <- (fun _ ->
                                                    
                                                        newPhotoImageData.Value <- string reader?result
                                                    )
                                                    reader?readAsDataURL file
                                            )

                                        ] []
                                    ]
                                button [
                                    attr.``class`` "bg-green-600 hover:bg-green-700 text-white rounded-xl px-5 py-3 font-medium shadow-md text-center"

                                    on.click (fun _ _ ->
                                        addGalleryPhoto()
                                    )
                                ] [
                                    text "Upload Photo"
                                ]
                            ]
                    ]

                div [attr.``class`` "flex flex-wrap gap-3 mb-6"]
                    [
                        button [
                            attr.``class`` (
                                if selectedGalleryAlbum.Value = AllPhotos then
                                    "px-4 py-2 rounded-xl bg-blue-600 text-white font-medium shadow scale-105 transition-all"
                                else 
                                    "px-4 py-2 rounded-xl bg-slate-200 text-slate-700 font-medium hover:scale-105 transition-all"
                            )

                            on.click (fun _ _ ->
                                selectedGalleryAlbum.Value <- AllPhotos
                            )
                        ] [
                            text "All"
                        ]

                        button [
                            attr.``class`` (
                                if selectedGalleryAlbum.Value = FamilyPhotos then
                                    "px-4 py-2 rounded-xl bg-pink-500 text-white font-medium shadow"
                                else   
                                    "px-4 py-2 rounded-xl bg-slate-200 text-slate-700 font-medium"
                            )
                            on.click (fun _ _ ->
                                selectedGalleryAlbum.Value <- FamilyPhotos
                            )
                        ] [
                            text "Family"
                        ]

                        button [
                            attr.``class`` (
                                if selectedGalleryAlbum.Value = GardenPhotos then
                                    "px-4 py-2 rounded-xl bg-green-600 text-white font-medium shadow"
                                else
                                    "px-4 py-2 rounded-xl bg-slate-200 text-slate-700 font-medium"
                            )
                            on.click (fun _ _ ->
                                selectedGalleryAlbum.Value <- GardenPhotos
                            )
                        ] [
                            text "Garden"
                        ]

                        button [
                            attr.``class`` (
                                if selectedGalleryAlbum.Value = BirthdayPhotos then
                                    "px-4 py-2 rounded-xl bg-orange-500 text-white font-medium shadow"
                                else
                                     "px-4 py-2 rounded-xl bg-slate-200 text-slate-700 font-medium"
                            )
                            on.click (fun _ _ ->
                                selectedGalleryAlbum.Value <- BirthdayPhotos
                            )
                        ] [
                            text "Birthday"
                        ]
                    ]

                Doc.BindView (fun selectedAlbum ->

                        let filteredPhotos =

                            galleryPhotos.Value
                            |> List.filter (fun photo ->

                                match selectedAlbum with
                                | AllPhotos -> true
                                | FamilyPhotos -> photo.Album = "Family"
                                | GardenPhotos -> photo.Album = "Garden"
                                | BirthdayPhotos -> photo.Album = "Birthday"
                            )

                        if not (List.isEmpty filteredPhotos) then

                            div [attr.``class`` "grid gap-6 md:grid-cols-3"]

                                [

                                    yield!

                                        filteredPhotos
                                        |> List.map (fun photo ->

                                            div [attr.``class`` "bg-white rounded-3xl shadow-lg p-5 border border-slate-100 hover:shadow-xl transition-all duration-300"]

                                                [

                                                    div [

                                                        attr.``class`` "h-48 rounded-2xl overflow-hidden bg-slate-100 mb-4 cursor-pointer"

                                                        on.click (fun _ _ ->
                                                            selectedFullscreenPhoto.Value <- Some photo
                                                        )

                                                    ]

                                                        [

                                                            if photo.ImageData.StartsWith("data:image") then

                                                                img [

                                                                    attr.src photo.ImageData
                                                                    attr.``class`` "w-full h-full object-cover"

                                                                ] []

                                                            else

                                                                div [

                                                                    attr.``class`` "w-full h-full flex items-center justify-center text-6xl"

                                                                ] [

                                                                    text photo.ImageData
                                                                ]
                                                        ]

                                                    h2 [attr.``class`` "text-xl font-semibold text-slate-800 mb-2"]

                                                        [
                                                            text photo.Title
                                                        ]

                                                    p [attr.``class`` "text-sm text-slate-500 mb-3"]
                                                        [
                                                            text ("Uploaded: " + photo.UploadedAt)
                                                        ]

                                                    div [attr.``class`` "flex items-center justify-between gap-3"]
                                                        [
                                                            span [

                                                                attr.``class`` "text-sm bg-blue-100 text-blue-700 px-4 py-2 rounded-full font-semibold"

                                                            ] [

                                                                text photo.Album
                                                            ]

                                                            button [
                                                                attr.``class`` "text-sm bg-red-100 text-red-700 px-4 py-2 rounded-full font-semibold hover:bg-red-200 transition-all"

                                                                on.click (fun _ ev ->
                                                                    ev.StopPropagation()
                                                                    deleteGalleryPhoto photo.Id
                                                                )
                                                            ] [
                                                                text "Delete"
                                                            ]
                                                        ]
                                                ]
                                        )
                                ]

                        else

                            div [attr.``class`` "grid gap-6 md:grid-cols-3"]
                                [
                            
                                    if selectedAlbum = AllPhotos || selectedAlbum = FamilyPhotos then
                                        div [attr.``class`` "bg-white rounded-3xl shadow-lg p-5 border border-slate-100 hover:shadow-xl transition-all duration-300"]
                                            [
                                                div [attr.``class`` "h-48 rounded-2xl bg-blue-50 flex items-center justify-center text-6xl mb-4"]
                                                    [text "📷"]

                                                h2 [attr.``class`` "text-xl font-semibold text-slate-800 mb-2"]
                                                    [text "Family photo"]

                                                p [attr.``class`` "text-slate-600 mb-4"]
                                                    [text "A new picture was uploaded by the family."]

                                                span [attr.``class`` "text-sm bg-blue-100 text-blue-700 px-4 py-2 rounded-full font-semibold"]
                                                    [text "Family"]
                                            ]

                                    if selectedAlbum = AllPhotos || selectedAlbum = GardenPhotos then
                                        div [attr.``class`` "bg-white rounded-3xl shadow-lg p-5 border border-slate-100 hover:shadow-xl transition-all duration-300"]
                                            [
                                                div [attr.``class`` "h-48 rounded-2xl bg-green-50 flex items-center justify-center text-6xl mb-4"]
                                                    [text "🌿"]

                                                h2 [attr.``class`` "text-xl font-semibold text-slate-800 mb-2"]
                                                    [text "Garden walk"]

                                                p [attr.``class`` "text-slate-600 mb-4"]
                                                    [text "A calm outdoor memory from a family visit."]

                                                span [attr.``class`` "text-sm bg-green-100 text-green-700 px-4 py-2 rounded-full font-semibold"]
                                                    [text "Garden"]
                                            ]

                                    if selectedAlbum = AllPhotos || selectedAlbum = BirthdayPhotos then
                                        div [attr.``class`` "bg-white rounded-3xl shadow-lg p-5 border border-slate-100 hover:shadow-xl transition-all duration-300"]
                                            [
                                                div [attr.``class`` "h-48 rounded-2xl bg-pink-50 flex items-center justify-center text-6xl mb-4"]
                                                    [text "🎂"]

                                                h2 [attr.``class`` "text-xl font-semibold text-slate-800 mb-2"]
                                                    [text "Birthday memory"]

                                                p [attr.``class`` "text-slate-600 mb-4"]
                                                    [text "A shared family celebration photo."]

                                                span [attr.``class`` "text-sm bg-pink-100 text-pink-700 px-4 py-2 rounded-full font-semibold"]
                                                    [text "Birthday"]
                                            ]
                                ]

                        ) selectedGalleryAlbum.View

                Doc.BindView (fun selectedPhoto ->

                    match selectedPhoto with
                    | Some photo ->

                        div [
                            attr.``class`` "fixed inset-0 bg-black bg-opacity-80 z-50 flex items-center justify-center p-4"
                        
                            on.click (fun _ _ ->
                                selectedFullscreenPhoto.Value <- None
                            )

                        ] [
                            div [

                                attr.``class`` "bg-white rounded-3xl p-4 max-w-4xl w-full relative"

                                on.click (fun _ ev ->
                                    ev.StopPropagation()
                                )

                            ]
                                [
                                    button [
                                        attr.``class`` "absolute top-4 right-4 bg-red-100 text-red-700 rounded-full px-4 py-2 font-bold hover:bg-red-200"

                                        on.click (fun _ _ ->
                                            selectedFullscreenPhoto.Value <- None
                                        )
                                    ] [
                                        text "X"
                                    ]

                                    img [
                                        attr.src photo.ImageData
                                        attr.``class`` "w-full max-h-[75vh] object-contain rounded-2xl"
                                    ] []

                                    h2 [attr.``class`` "text-2xl font-bold text-slate-800 mt-4"]
                                        [text photo.Title]

                                    p [attr.``class`` "text-slate-500 mt-1"]
                                        [text photo.Album]
                                ]
                        ]

                    | None ->
                        Doc.Empty

                ) selectedFullscreenPhoto.View

                    ]

        | ProfilePage ->
            div [] [
                h1 [attr.``class`` "text-3xl font-bold mb-2 text-slate-800"]
                    [text "Personal Profile"]

                p [attr.``class`` "text-slate-600 mb-6"]
                    [text "Important personal and health related information"]

                div [attr.``class`` "grid gap-6 md:grid-cols-2"]

                    [
                        div [attr.``class`` "bg-white rounded-2xl shadow p-6"]
                            [
                                h2 [attr.``class`` "text-2xl font-semibold mb-5 text-slate-800"]
                                    [text "Personal Information"]

                                div [attr.``class`` "space-y-4"]
                                    [
                                        Doc.Input [
                                            attr.placeholder "Full name"
                                            attr.``class`` "w-full border border-slate-200 rounded-xl px-4 py-3"
                                        ] profileName

                                        Doc.Input [
                                            attr.placeholder "Birth date (YYYY-MM-DD)"
                                            attr.``class`` "w-full border border-slate-200 rounded-xl px-4 py-3"
                                        ] profileBirthDate

                                        div [attr.``class`` "bg-slate-100 rounded-xl px-4 py-3 text-slate-700 font-medium"]
                                            [
                                                 Doc.BindView (fun (birthDate: string) ->

                                                    let currentYear =
                                                        System.DateTime.Now.Year

                                                    let ageText =

                                                        if birthDate.Length >= 4 then

                                                            let birthYear =
                                                                birthDate.Substring(0, 4)

                                                            match System.Int32.TryParse(birthYear) with
                                                            | true, year ->
                                                                "Age: " + string (currentYear - year) + " years"

                                                            | _ ->
                                                                "Age: -"

                                                        else
                                                            "Age: -"

                                                    text ageText

                                                ) profileBirthDate.View

                                            ]
                                    

                                        Doc.Input [
                                            attr.placeholder "Height"
                                            attr.``class`` "w-full border border-slate-200 rounded-xl px-4 py-3"
                                        ] profileHeight

                                        Doc.Input [
                                            attr.placeholder "Weight"
                                            attr.``class`` "w-full border border-slate-200 rounded-xl px-4 py-3"
                                        ] profileWeight
                                            ]
                            ]
                        div [attr.``class`` "bg-white rounded-2xl shadow p-6"]

                            [

                                h2 [attr.``class`` "text-2xl font-semibold mb-5 text-slate-800"]
                                    [text "Emergency & Health"]

                                div [attr.``class`` "space-y-4"]

                                    [
                                        Doc.Input [
                                            attr.placeholder "Emergency contact"
                                            attr.``class`` "w-full border border-slate-200 rounded-xl px-4 py-3"
                                        ] emergencyContact

                                        Doc.Input [
                                            attr.placeholder "Phone number"
                                            attr.``class`` "w-full border border-slate-200 rounded-xl px-4 py-3"
                                        ] emergencyPhone

                                        Doc.InputArea [
                                            attr.placeholder "Medical notes"
                                            attr.``class`` "w-full border border-slate-200 rounded-xl px-4 py-3 h-32"
                                        ] medicalNotes

                                        button [
                                            attr.``class`` "w-full bg-blue-600 hover:bg-blue-700 transition-all text-white rounded-xl px-5 py-3 font-medium shadow-md mt-4"

                                            on.click (fun _ _ ->
                                                saveProfile()
                                            )
                                        ] [
                                            text "Save Profile"
                                        ]
                                    ]
                            ]
                    ]
            ]

        | SettingsPage ->
            div [] [
                h1 [attr.``class`` "text-3xl font-bold mb-2 text-slate-800"]
                    [text "Settings"]

                p [attr.``class`` "text-slate-600 mb-6"]
                    [text "Accessibility and reminder preferences for elderly users."]

                div [attr.``class`` "grid gap-4"]
                    [
                        settingToggle
                            "Large text mode"
                            "Makes the interface easier to read for elderly users."
                            largeTextMode

                        settingToggle
                            "High contrast mode"
                            "Improves visibility by preparing stronger contrast options."
                            highContrastMode

                        settingToggle
                            "Show reminders"
                            "Controls whether care and medication reminders should be visible."
                            showReminders
                    ]

                div [attr.``class`` "mt-6 bg-blue-50 border border-blue-100 rounded-2xl p-5"]
                    [
                        h2 [attr.``class`` "text-xl font-semibold text-blue-800 mb-2"]
                            [text "Accessibility note"]

                        p [attr.``class`` "text-blue-700"]
                            [text "These settings prepare the application for elderly-friendly usability improvements, such as larger text, better contrast and reminder visibility."]
                    ]
            ]

    let navbar = 
        nav [attr.``class`` "bg-white shadow-md"]
            [

                div [attr.``class`` "max-w-7xl mx-auto px-4"]
                    [

                        div [attr.``class`` "flex justify-between items-center h-16"]
                            [

                                h1 [attr.``class`` "text-xl font-bold text-slate-800"]
                                    [
                                        text "Senior Care Companion"
                                    ]

                                div [attr.``class`` "hidden md:flex gap-6"]
                                    [

                                        button [
                                            attr.``class`` (navButtonClass selectedAppPage.Value DashboardPage)
                                            on.click (fun _ _ ->
                                                selectedAppPage.Value <- DashboardPage
                                            )
                                        ] [
                                            text "Dashboard"
                                        ]

                                        button [
                                            attr.``class`` (navButtonClass selectedAppPage.Value TasksPage)
                                            on.click (fun _ _ ->
                                                selectedAppPage.Value <- TasksPage
                                            )
                                        ] [
                                            text "Tasks"
                                        ]

                                        button [
                                            attr.``class`` (navButtonClass selectedAppPage.Value GalleryPage)
                                            on.click (fun _ _ ->
                                                selectedAppPage.Value <- GalleryPage
                                            )
                                        ] [
                                            text "Gallery"
                                        ]

                                        button [
                                            attr.``class`` (navButtonClass selectedAppPage.Value ProfilePage)
                                            on.click (fun _ _ ->
                                                selectedAppPage.Value <- ProfilePage
                                            )
                                        ] [
                                            text "Profile"
                                        ]

                                        button [
                                            attr.``class`` (navButtonClass selectedAppPage.Value SettingsPage)
                                            on.click (fun _ _ ->
                                                selectedAppPage.Value <- SettingsPage
                                            )
                                        ] [
                                            text "Settings"
                                        ]
                                    ]

                                button [
                                    attr.``class`` "md:hidden text-slate-700 text-2xl"

                                    on.click (fun _ _ ->
                                        mobileMenuOpen.Value <- not mobileMenuOpen.Value
                                    )
                                ] [
                                    text "☰"
                                ]
                            ]
                    ]

                Doc.BindView (fun isOpen ->

                    if isOpen then

                        div [attr.``class`` "md:hidden bg-white border-t border-slate-200 px-4 py-4 flex flex-col gap-4"]
                            [

                                button [
                                    attr.``class`` "text-left text-slate-700 font-medium"

                                    on.click (fun _ _ ->
                                        selectedAppPage.Value <- DashboardPage
                                        mobileMenuOpen.Value <- false
                                    )
                                ] [
                                    text "Dashboard"
                                ]

                                button [
                                    attr.``class`` "text-left text-slate-700 font-medium"

                                    on.click (fun _ _ ->
                                        selectedAppPage.Value <- TasksPage
                                        mobileMenuOpen.Value <- false
                                    )
                                ] [
                                    text "Tasks"
                                ]

                                button [
                                    attr.``class`` "text-left text-slate-700 font-medium"

                                    on.click (fun _ _ ->
                                        selectedAppPage.Value <- GalleryPage
                                        mobileMenuOpen.Value <- false
                                    )
                                ] [
                                    text "Gallery"
                                ]

                                button [
                                    attr.``class`` "text-left text-slate-700 font-medium"

                                    on.click (fun _ _ ->
                                        selectedAppPage.Value <- ProfilePage
                                        mobileMenuOpen.Value <- false
                                    )
                                ] [
                                    text "Profile"
                                ]
                                button [
                                    attr.``class`` "text-left text-slate-700 font-medium"

                                    on.click (fun _ _ ->
                                        selectedAppPage.Value <- SettingsPage
                                        mobileMenuOpen.Value <- false
                                    )
                                ] [
                                    text "Settings"
                                ]
                            ]
                    
                    else
                        Doc.Empty

                ) mobileMenuOpen.View
            ]

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
                        Doc.BindView renderAppPage selectedAppPage.View
                    ]
                }
        (*              
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
                                        Doc.Input [
                                            attr.placeholder "Task name"
                                            attr.``class`` "border  border-slate-200 rounded-xl px-4 py-3 shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-400"
                                        ] newTitle


                                        select [
                                            attr.``class`` "border rounded-slate-200 rounded-xl px-4 py-3 bg-white text-slate-700 shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-400"
                                            on.change (fun el _ ->
                                                newTime.Value <- el?value
                                            )
                                        ] [
                                            option [
                                                attr.value ""
                                                attr.selected "selected"
                                                attr.disabled "disabled"
                                            ] [
                                                text "Select time"
                                            ]
                                            option [attr.value "06:00"] [text "06:00"]
                                            option [attr.value "06:30"] [text "06:30"]
                                            option [attr.value "07:00"] [text "07:00"]
                                            option [attr.value "07:30"] [text "07:30"]
                                            option [attr.value "08:00"] [text "08:00"]
                                            option [attr.value "08:30"] [text "08:30"]
                                            option [attr.value "09:00"] [text "09:00"]
                                            option [attr.value "09:30"] [text "09:30"]
                                            option [attr.value "10:00"] [text "10:00"]
                                            option [attr.value "10:30"] [text "10:30"]
                                            option [attr.value "11:00"] [text "11:00"]
                                            option [attr.value "11:30"] [text "11:30"]
                                            option [attr.value "12:00"] [text "12:00"]
                                            option [attr.value "12:30"] [text "12:30"]
                                            option [attr.value "13:00"] [text "13:00"]
                                            option [attr.value "13:30"] [text "13:30"]
                                            option [attr.value "14:00"] [text "14:00"]
                                            option [attr.value "14:30"] [text "14:30"]
                                            option [attr.value "15:00"] [text "15:00"]
                                            option [attr.value "15:30"] [text "15:30"]
                                            option [attr.value "16:00"] [text "16:00"]
                                            option [attr.value "16:30"] [text "16:30"]
                                            option [attr.value "17:00"] [text "17:00"]
                                            option [attr.value "17:30"] [text "17:30"]
                                            option [attr.value "18:00"] [text "18:00"]
                                            option [attr.value "18:30"] [text "18:30"]
                                            option [attr.value "19:00"] [text "19:00"]
                                            option [attr.value "19:30"] [text "19:30"]
                                            option [attr.value "20:00"] [text "20:00"]
                                            option [attr.value "20:30"] [text "20:30"]
                                            option [attr.value "21:00"] [text "21:00"]
                                            option [attr.value "21:30"] [text "21:30"]
                                            option [attr.value "22:00"] [text "22:00"]
                                        ]

                                        select [
                                            attr.``class`` "border border-slate-200 rounded-xl px-4 py-3 bg-white text-slate-700 shadow-sm focus:ring-2 focus:ring-blue-400"
                                            on.change (fun el _ ->
                                                newCategory.Value <- el?value
                                            )
                                        ] [
                                            option [
                                                attr.value ""
                                                attr.selected "selected"
                                                attr.disabled "disabled"
                                                ] [
                                                    text "Select category"
                                                ]
                                            option [attr.value "Medication"] [text "Medication"]
                                            option [attr.value "Health"] [text "Health"]
                                            option [attr.value "Appointment"] [text "Appointment"]
                                            option [attr.value "Family"] [text "Family"]
                                        ]

                                        button [
                                            attr.``class`` "bg-blue-600 hover:bg-blue-700 transition-all text-white rounded-xl px-5 py-3 font-medium shadow-md"
                                            on.click (fun _ _ -> addTask())
                                        ] [
                                            text "Add"
                                        ]
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
          *)  

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
        loadProfile()
        loadTasks()
        loadSettings()
        loadGalleryPhotos()

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
            .Navbar(navbar)
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
