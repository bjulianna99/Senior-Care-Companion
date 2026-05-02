namespace DUE_FSharp_SPASandbox_2026

open WebSharper

[<JavaScript>]
type Person =
    {
        FirstName: string
        LastName: string
        Age: int
    }

module Server =

    [<Rpc>]
    // Returns an option, None if failed.
    let SavePerson (p: Person) =
        // TODO: do the actual saving
        async.Return (Some p)

