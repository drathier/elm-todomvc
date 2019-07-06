module Frontend exposing (app)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

1.  Model - a full definition of the application's state
2.  Update - a way to step the application state forward
3.  View - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>

-}

import Browser.Dom as Dom
import Browser.Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Lamdera.Frontend
import Msg exposing (Entry, FrontendMsg(..), Model, ToBackend(..), ToFrontend(..), emptyModel, newEntry)
import Task
import Url


app =
    Lamdera.Frontend.application
        { init = init
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        , update = updateWithStorage
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = \model -> { title = "Lamdera • Elm • TodoMVC", body = [ view model ] }
        }


type alias Id =
    Int


setStorage : Model -> Cmd FrontendMsg
setStorage model =
    Lamdera.Frontend.sendToBackend 5000 SendToBackendFeedback (SetStorage model.entries)


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , if model.entries == newModel.entries then
        -- nothing important changed, no need to tell the backend :)
        Cmd.none

      else
        Cmd.batch [ setStorage newModel, cmds ]
    )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NewState entries ->
            -- TODO: merge the two models somehow; this should probably be an add-remove crdt set
            ( { model | entries = entries }, Cmd.none )


init : Url.Url -> Browser.Navigation.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( emptyModel
    , Lamdera.Frontend.sendToBackend 5000 SendToBackendFeedback ClientJoined
    )



-- UPDATE
-- How we update our Model on a given Msg?


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Add ->
            ( { model
                | field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries

                    else
                        model.entries ++ [ newEntry model.field ]
              }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        EditingEntry id isEditing ->
            let
                updateEntry entryId t =
                    if entryId == id then
                        { t | editing = isEditing }

                    else
                        t

                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | entries = List.indexedMap updateEntry model.entries }
            , Task.attempt (\_ -> NoOp) focus
            )

        UpdateEntry id task ->
            let
                updateEntry entryId t =
                    if entryId == id then
                        { t | description = task }

                    else
                        t
            in
            ( { model | entries = List.indexedMap updateEntry model.entries }
            , Cmd.none
            )

        Delete id ->
            ( { model
                | entries =
                    model.entries
                        |> List.indexedMap
                            (\idx v ->
                                if idx == id then
                                    []

                                else
                                    [ v ]
                            )
                        |> List.concat
              }
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | entries = List.filter (not << .completed) model.entries }
            , Cmd.none
            )

        Check id isCompleted ->
            let
                updateEntry entryId t =
                    if entryId == id then
                        { t | completed = isCompleted }

                    else
                        t
            in
            ( { model | entries = List.indexedMap updateEntry model.entries }
            , Cmd.none
            )

        CheckAll isCompleted ->
            let
                updateEntry t =
                    { t | completed = isCompleted }
            in
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )

        SendToBackendFeedback _ ->
            -- assume it went ok :)
            -- TODO: don't assume it went ok
            ( model, Cmd.none )



-- VIEW


view : Model -> Html FrontendMsg
view model =
    div []
        [ node "link" [ rel "stylesheet", href "/style.css" ] []
        , div
            [ class "todomvc-wrapper"
            , style "visibility" "hidden"
            ]
            [ section
                [ class "todoapp" ]
                [ lazy viewInput model.field
                , lazy2 viewEntries model.visibility model.entries
                , lazy2 viewControls model.visibility model.entries
                ]
            , infoFooter
            ]
        ]


viewInput : String -> Html FrontendMsg
viewInput task =
    header
        [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : FrontendMsg -> Attribute FrontendMsg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)



-- VIEW ALL ENTRIES


viewEntries : String -> List Entry -> Html FrontendMsg
viewEntries visibility entries =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True

        allCompleted =
            List.all .completed entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"

            else
                "visible"
    in
    section
        [ class "main"
        , style "visibility" cssVisibility
        ]
        [ input
            [ class "toggle-all"
            , type_ "checkbox"
            , name "toggle"
            , checked allCompleted
            , onClick (CheckAll (not allCompleted))
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list" ] <|
            List.indexedMap viewKeyedEntry (List.filter isVisible entries)
        ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Id -> Entry -> ( String, Html FrontendMsg )
viewKeyedEntry id todo =
    ( String.fromInt id, lazy (viewEntry id) todo )


viewEntry : Id -> Entry -> Html FrontendMsg
viewEntry entryId todo =
    li
        [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick (Check entryId (not todo.completed))
                ]
                []
            , label
                [ onDoubleClick (EditingEntry entryId True) ]
                [ text todo.description ]
            , button
                [ class "destroy"
                , onClick (Delete entryId)
                ]
                []
            ]
        , input
            [ class "edit"
            , value todo.description
            , name "title"
            , id ("todo-" ++ String.fromInt entryId)
            , onInput (UpdateEntry entryId)
            , onBlur (EditingEntry entryId False)
            , onEnter (EditingEntry entryId False)
            ]
            []
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : String -> List Entry -> Html FrontendMsg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ lazy viewControlsCount entriesLeft
        , lazy viewControlsFilters visibility
        , lazy viewControlsClear entriesCompleted
        ]


viewControlsCount : Id -> Html FrontendMsg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"

            else
                " items"
    in
    span
        [ class "todo-count" ]
        [ strong [] [ text (String.fromInt entriesLeft) ]
        , text (item_ ++ " left")
        ]


viewControlsFilters : String -> Html FrontendMsg
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]


visibilitySwap : String -> String -> String -> Html FrontendMsg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text visibility ]
        ]


viewControlsClear : Id -> Html FrontendMsg
viewControlsClear entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        , p []
            [ text "Modified to run on "
            , a [ href "http://lamdera.com" ] [ text "Lamdera" ]
            ]
        ]
