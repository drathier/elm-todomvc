module Msg exposing (BackendMsg(..), Entry, FrontendMsg(..), Model, ToBackend(..), ToFrontend(..), emptyModel, newEntry)

import Lamdera.Types exposing (..)


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type FrontendMsg
    = NoOp
    | UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String
    | SendToBackendFeedback (Result WsError ())


type ToBackend
    = ClientJoined
    | SetStorage (List Entry)


type BackendMsg
    = SendToFrontendFeedback ClientId (Result WsError ())


type ToFrontend
    = NewState (List Entry)



-- MODEL
-- The full application state of our todo app.


type alias Model =
    { entries : List Entry
    , field : String
    , visibility : String
    }


type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    }


emptyModel : Model
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    }


newEntry : String -> Entry
newEntry desc =
    { description = desc
    , completed = False
    , editing = False
    }
