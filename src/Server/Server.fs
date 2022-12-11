module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module Storage =
    type Model = { Todos: Todo list; Cards: Card list }

    let todos = ResizeArray()
    let cards = ResizeArray()

    let addTodo (todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"

    let addCard (card: Card) =
        cards.Add card
        Ok()

    do
        addTodo (Todo.create "Create new SAFE project") |> ignore
        addTodo (Todo.create "Write your app") |> ignore
        addTodo (Todo.create "Ship it !!!") |> ignore

let todosApi = {
    getTodos = fun () -> async { return Storage.todos |> List.ofSeq }
    getCards = fun () -> async { return Storage.cards |> List.ofSeq }
    addTodo =
        fun todo ->
            async {
                return
                    match Storage.addTodo todo with
                    | Ok () -> todo
                    | Error e -> failwith e
            }
    addCard =
        fun card ->
            async {
                return
                    match Storage.addCard card with
                    | Ok () -> card
                    | Error e -> failwith e
            }
}

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue todosApi
    |> Remoting.buildHttpHandler

let app =
    application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0