module Backend exposing (app)

import Lamdera.Backend
import Lamdera.Types exposing (ClientId)
import Msg exposing (..)
import Set exposing (Set)


type alias Model =
    { entries : List Entry
    , clients : Set ClientId
    }


init : ( Model, Cmd BackendMsg )
init =
    ( { clients = Set.empty, entries = [] }, Cmd.none )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        SendToFrontendFeedback clientId (Err _) ->
            ( { model | clients = Set.remove clientId model.clients }, Cmd.none )

        SendToFrontendFeedback _ (Ok ()) ->
            ( model, Cmd.none )


updateFromFrontend : ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend clientId msg model =
    case msg of
        ClientJoined ->
            ( { model | clients = Set.insert clientId model.clients }
            , Msg.sendToFrontend 5000 clientId (SendToFrontendFeedback clientId) (NewState model.entries)
            )

        SetStorage newEntries ->
            ( { model | entries = newEntries }
            , broadcastNewOps clientId model.clients newEntries
            )


broadcastNewOps origin clients newOps =
    clients
        |> Set.toList
        -- don't send changes back to origin; we don't have a crdt so this will only cause laggy behaviour
        |> List.filter ((/=) origin)
        |> List.map (\cid -> Msg.sendToFrontend 5000 cid (SendToFrontendFeedback cid) (NewState newOps))
        |> Cmd.batch


app =
    Lamdera.Backend.application
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }
