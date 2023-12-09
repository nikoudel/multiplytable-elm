module View exposing (view)

import AppLogic exposing (..)
import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Model exposing (..)


view : Model -> Html Msg
view model =
    div []
        -- [ h3 [ id "debug" ] [ Maybe.withDefault "" model.debugText |> text ]
        [ div [ id "main" ]
            [ div [ id "grid" ] [ viewTable model.cells ]
            , div [ id "input" ] [ viewInput model ]
            ]
        , div [ id "right" ]
            [ div [ id "buttons" ]
                [ img [ src (modeImageUrl model), onClick ToggleMode ] []
                , img
                    [ src (resetImageUrl model)
                    , onMouseDown ResetDown
                    , onMouseUp ResetModel
                    , onDragEnd ResetModel
                    ]
                    []
                , h1 [ class (errorCountClass model) ] [ text (model.errorCount |> String.fromInt) ]
                , div [] (List.indexedMap checkboxToHtml (Array.toList model.checkboxes))
                ]
            ]
        ]


viewTable : Array Cell -> Html Msg
viewTable cells =
    let
        rows =
            split 9 [] (Array.toList cells)

        rowToHtml row =
            tr [] (List.map cellToHtml row)

        cellToHtml cell =
            td [ class (cellClass cell) ]
                [ text cell.content
                , img
                    [ class
                        (if cell.imageHidden then
                            "hidden"

                         else
                            ""
                        )
                    , src cell.imageSrc
                    , onMouseEnter (MouseEnter cell.index)
                    , onMouseLeave (MouseLeave cell.index)
                    ]
                    []
                ]
    in
    table [] [ tbody [] (List.map rowToHtml rows) ]


viewInput : Model -> Html Msg
viewInput model =
    input
        [ id theInputId
        , type_ "text"
        , autofocus True
        , onBlur Focus
        , disabled (model.selectedCell < 0)
        , value model.inputText
        , onKeyDown UserInput
        ]
        []


checkboxToHtml : Int -> Bool -> Html Msg
checkboxToHtml i isChecked =
    div []
        [ label []
            [ text (i + 2 |> String.fromInt)
            , text " "
            , input
                [ type_ "checkbox"
                , checked isChecked
                , onClick (ToggleCheckbox i)
                ]
                []
            ]
        ]


onDragEnd : Msg -> Attribute Msg
onDragEnd msg =
    on "dragend" (Json.succeed msg)


onKeyDown : (Int -> Msg) -> Attribute Msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)
