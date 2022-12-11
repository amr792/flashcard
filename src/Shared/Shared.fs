namespace Shared

open System

type Todo = { Id: Guid; Description: string }
type Card = { Id: Guid; Front: string; Back: string }
type Deck = { Id: Guid; Name: string }
type ITodosApi = {
        getTodos: unit -> Async<Todo list>
        addTodo: Todo -> Async<Todo>
        getCards: unit -> Async<Card list>
        addCard: Card -> Async<Card>
    }

module Card =
    let create (front: string) (back: string) =
        { Id = Guid.NewGuid()
          Front = front
          Back = back }

module Deck =
    let create (name: string) =
        { Id = Guid.NewGuid()
          Name = name }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName