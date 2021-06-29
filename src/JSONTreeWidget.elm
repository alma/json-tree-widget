module JSONTreeWidget exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import JsonTree exposing (TaggedValue(..))


type alias Flags =
    { json : String, showToolbar : Bool, showSearchbar : Bool }


type alias Model =
    { json : String
    , parseResult : Result Decode.Error JsonTree.Node
    , treeState : JsonTree.State
    , showToolbar : Bool
    , showSearchbar : Bool
    , query : Maybe String
    }


type Msg
    = Parse
    | SetTreeViewState JsonTree.State
    | ExpandAll
    | CollapseAll
    | SearchUpdate String


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { json = flags.json
      , parseResult = JsonTree.parseString flags.json
      , treeState = JsonTree.defaultState
      , showToolbar = flags.showToolbar
      , showSearchbar = flags.showToolbar
      , query = Nothing
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

        SearchUpdate query ->
            if query == "" then
                ( { model | query = Nothing }, Cmd.none )

            else
                ( { model | query = Just query }, Cmd.none )


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
            fieldset [ class "well well-sm form-inline" ]
                [ div
                    [ class "form-group pull-right" ]
                    [ div [ class "btn-group" ]
                        [ button [ type_ "button", class "btn btn-default", onClick ExpandAll ]
                            [ span [ class "glyphicon glyphicon-plus", style "margin-right" "6px" ] []
                            , text "Expand All"
                            ]
                        , text "\u{00A0}"
                        , button [ type_ "button", class "btn btn-default", onClick CollapseAll ]
                            [ span [ class "glyphicon glyphicon-minus", style "margin-right" "6px" ] []
                            , text "Collapse All"
                            ]
                        ]
                    ]
                , div [ class "form-group pull-left", style "margin-left" "1em" ]
                    [ label [ for "filter-json" ] [ text "Filter:\u{00A0}" ]
                    , if model.showSearchbar then
                        input [ id "filter-json", type_ "search", placeholder "Type to filter json keys", class "form-control", style "width" "300px", onInput SearchUpdate ] []

                      else
                        text ""
                    ]
                ]

        config =
            { colors = JsonTree.defaultColors, onSelect = Nothing, toMsg = SetTreeViewState }
    in
    div []
        [ if model.showToolbar then
            toolbar

          else
            text ""
        , br [] []
        , case model.parseResult of
            Ok rootNode ->
                JsonTree.view rootNode config model.treeState model.query

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
