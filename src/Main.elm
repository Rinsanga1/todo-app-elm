module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Todo =
    { id : Int
    , text : String
    , done : Bool
    }

type Filter
    = All
    | Active
    | Completed

type alias Model =
    { todos : List Todo
    , nextId : Int
    , input : String
    , filter : Filter
    }

init : Model
init =
    { todos = []
    , nextId = 1
    , input = ""
    , filter = All
    }


-- UPDATE

type Msg
    = UpdateInput String
    | AddTodo
    | Toggle Int
    | Delete Int
    | SetFilter Filter
    | ClearCompleted

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput value ->
            { model | input = value }

        AddTodo ->
            let
                trimmed = String.trim model.input
            in
            if String.isEmpty trimmed then
                model
            else
                { model
                    | todos = model.todos ++ [ { id = model.nextId, text = trimmed, done = False } ]
                    , nextId = model.nextId + 1
                    , input = ""
                }

        Toggle id ->
            { model
                | todos =
                    List.map
                        (\t -> if t.id == id then { t | done = not t.done } else t)
                        model.todos
            }

        Delete id ->
            { model | todos = List.filter (\t -> t.id /= id) model.todos }

        SetFilter filter ->
            { model | filter = filter }

        ClearCompleted ->
            { model | todos = List.filter (\t -> not t.done) model.todos }


-- VIEW

view : Model -> Html Msg
view model =
    let
        visibleTodos =
            List.filter (matchesFilter model.filter) model.todos

        remaining =
            List.length (List.filter (\t -> not t.done) model.todos)

        anyDone =
            List.any (\t -> t.done) model.todos
    in
    div [ class "app" ]
        [ viewHeader
        , viewInput model.input
        , viewFilters model.filter
        , viewTodoList visibleTodos
        , viewFooter remaining anyDone
        ]

matchesFilter : Filter -> Todo -> Bool
matchesFilter filter todo =
    case filter of
        All       -> True
        Active    -> not todo.done
        Completed -> todo.done

viewHeader : Html Msg
viewHeader =
    div [ class "header" ]
        [ h1 [] [ text "// todos" ]
        , span [ class "badge" ] [ text "elm" ]
        ]

viewInput : String -> Html Msg
viewInput inputValue =
    div [ class "input-row" ]
        [ input
            [ type_ "text"
            , placeholder "What needs doing?"
            , value inputValue
            , onInput UpdateInput
            , onEnter AddTodo
            , autofocus True
            ]
            []
        , button [ onClick AddTodo, class "btn-add" ] [ text "ADD" ]
        ]

viewFilters : Filter -> Html Msg
viewFilters current =
    div [ class "filters" ]
        [ filterButton "All"       All       current
        , filterButton "Active"    Active    current
        , filterButton "Completed" Completed current
        ]

filterButton : String -> Filter -> Filter -> Html Msg
filterButton label filter current =
    button
        [ onClick (SetFilter filter)
        , classList [ ( "filter-btn", True ), ( "active", filter == current ) ]
        ]
        [ text label ]

viewTodoList : List Todo -> Html Msg
viewTodoList todos =
    if List.isEmpty todos then
        div [ class "empty" ] [ text "— nothing here —" ]
    else
        div [ class "todo-list" ] (List.map viewTodoItem todos)

viewTodoItem : Todo -> Html Msg
viewTodoItem todo =
    div [ classList [ ( "todo-item", True ), ( "done", todo.done ) ] ]
        [ button
            [ classList [ ( "check-box", True ), ( "checked", todo.done ) ]
            , onClick (Toggle todo.id)
            ]
            [ if todo.done then text "✓" else text "" ]
        , span [ class "todo-text" ] [ text todo.text ]
        , button [ class "btn-delete", onClick (Delete todo.id) ] [ text "✕" ]
        ]

viewFooter : Int -> Bool -> Html Msg
viewFooter remaining anyDone =
    div [ class "footer" ]
        [ span []
            [ text (String.fromInt remaining ++ " item" ++ (if remaining == 1 then "" else "s") ++ " left") ]
        , if anyDone then
            button [ class "btn-clear", onClick ClearCompleted ] [ text "clear completed" ]
          else
            text ""
        ]


-- HELPERS

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not Enter"
    in
    on "keydown" (Json.Decode.andThen isEnter keyCode)
