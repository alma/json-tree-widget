module JSONTreeWidget exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import JsonTree


type alias Flags =
    { json : String, showToolbar : Bool }


type alias Model =
    { json : String
    , parseResult : Result Decode.Error JsonTree.Node
    , treeState : JsonTree.State
    , showToolbar : Bool
    }


type Msg
    = Parse
    | SetTreeViewState JsonTree.State
    | ExpandAll
    | CollapseAll


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { json = flags.json
      , parseResult = JsonTree.parseString flags.json
      , treeState = JsonTree.defaultState
      , showToolbar = flags.showToolbar
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Parse ->
            ( { model | parseResult = JsonTree.parseString model.json }, Cmd.none )

        SetTreeViewState state ->
            ( { model | treeState = state }, Cmd.none )

        ExpandAll ->
            ( { model | treeState = JsonTree.expandAll model.treeState }, Cmd.none )

        CollapseAll ->
            case model.parseResult of
                Ok rootNode ->
                    ( { model | treeState = JsonTree.collapseToDepth 1 rootNode model.treeState }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ style "margin" "20px" ] [ viewJsonTree model ]


viewJsonTree : Model -> Html Msg
viewJsonTree model =
    let
        toolbar =
            div []
                [ button [ onClick ExpandAll ] [ text "Expand All" ]
                , button [ onClick CollapseAll ] [ text "Collapse All" ]
                ]

        config =
            { onSelect = Nothing, toMsg = SetTreeViewState }
    in
    div []
        [ if model.showToolbar then
            toolbar

          else
            text ""
        , br [] []
        , case model.parseResult of
            Ok rootNode ->
                JsonTree.view rootNode config model.treeState

            Err e ->
                pre [] [ text ("Invalid JSON: " ++ Decode.errorToString e) ]
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
