module Backend exposing (app)

import Lamdera.Backend
import Lamdera.Types exposing (ClientId)
import Msg exposing (..)
import Set exposing (Set)


type alias Model =
    { model : Msg.Model
    , clients : Set ClientId
    }


init : ( Model, Cmd BackendMsg )
init =
    ( { clients = Set.empty, model = Msg.emptyModel }, Cmd.none )


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
            , Msg.sendToFrontend 5000 clientId (SendToFrontendFeedback clientId) (NewState model.model)
            )

        SetStorage newModel ->
            ( { model | model = newModel }
            , broadcastNewOps model.clients newModel
            )


broadcastNewOps clients newOps =
    clients
        |> Set.toList
        |> List.map (\cid -> Msg.sendToFrontend 5000 cid (SendToFrontendFeedback cid) (NewState newOps))
        |> Cmd.batch


app =
    Lamdera.Backend.application
        { init = init
        , update = update
        , subscriptions = \m -> Sub.none
        , updateFromFrontend = updateFromFrontend
        }
