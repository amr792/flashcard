module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = { Todos: Todo list; Cards: Card list; Input: string; CardFrontInput: string; CardBackInput: string }

type Msg =
    | GotTodos of Todo list
    | GotCards of Card list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    | SetCardFrontInput of string
    | SetCardBackInput of string
    | AddCard
    | AddedCard of Card

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { Todos = []; Cards = []; Input = ""; CardFrontInput = ""; CardBackInput = "" }
    let cmd =
        Cmd.batch [
            Cmd.OfAsync.perform todosApi.getTodos () GotTodos
            Cmd.OfAsync.perform todosApi.getCards () GotCards
        ]

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | GotCards cards -> { model with Cards = cards }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input
        let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
    | AddedTodo todo -> { model with Todos = model.Todos @ [ todo ] }, Cmd.none
    | SetCardFrontInput value -> { model with CardFrontInput = value }, Cmd.none
    | SetCardBackInput value -> { model with CardBackInput = value }, Cmd.none
    | AddCard ->
        let card = Card.create model.CardFrontInput model.CardBackInput
        let cmd = Cmd.OfAsync.perform todosApi.addCard card AddedCard
        { model with CardFrontInput = ""; CardBackInput = "" }, cmd
    | AddedCard card -> { model with Cards = model.Cards @ [ card ] }, Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li [ prop.text todo.Description ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "What needs to be done?"
                            prop.onChange (fun x -> SetInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
        Bulma.field.div [
            for card in model.Cards do
                Bulma.control.div [
                    Bulma.card [
                        prop.text card.Front
                    ]
                    Bulma.card [
                        prop.text card.Back
                    ]
                ]
        ]
        Bulma.field.div [
            prop.children [
                Bulma.control.div [
                    Bulma.input.text [
                        prop.value model.CardFrontInput
                        prop.placeholder "Front"
                        prop.onChange ( fun s -> SetCardFrontInput s |> dispatch )
                    ]
                    Bulma.input.text [
                        prop.value model.CardBackInput
                        prop.placeholder "Back"
                        prop.onChange ( fun s -> SetCardBackInput s |> dispatch )
                    ]
                    Bulma.button.a [
                        prop.text "Create Card"
                        prop.onClick (fun _ -> dispatch AddCard)
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "flashcard"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]