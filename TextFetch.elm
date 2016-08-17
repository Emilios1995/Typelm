module TextFetch exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Json.Decode
import Http
import Task
import String
import Char
import Json.Decode exposing (at, string)
import Debug exposing (log)
import Utils
import List


type alias Model =
    { options : List Option
    , selectedText : Maybe Text
    , page : Maybe Int
    }


type alias Option =
    { url : String
    , name : String
    }


type alias Text =
    List (List Char)


type Msg
    = Fetch String
    | FetchSuccess Text
    | FetchFailure Http.Error
    | TurnPage


wordsPerPage =
    20


update : Msg -> Model -> ( Model, Cmd Msg, Maybe (List Char) )
update msg model =
    case msg of
        Fetch url ->
            ( model, getText url, Nothing )

        FetchSuccess text ->
            ( { model | page = (Just 1), selectedText = (Just text) }
            , Cmd.none
            , Just (text |> (List.take wordsPerPage) |> Utils.wordsToChars)
            )

        FetchFailure _ ->
            ( model, Cmd.none, Nothing )

        TurnPage ->
            case ( model.selectedText, model.page ) of
                ( Just text, Just page ) ->
                    ( { model | page = Just (page + 1) }
                    , Cmd.none
                    , let
                        newText =
                            text
                                |> List.drop (page * wordsPerPage)
                                |> List.take wordsPerPage
                                |> Utils.wordsToChars
                      in
                        if List.length newText > 0 then
                            Just newText
                        else
                            -- text is over
                            Nothing
                    )

                ( _, _ ) ->
                    ( model, Cmd.none, Nothing )


selectedOptionIdDecoder =
    at [ "target", "selectedOptions", "0", "id" ] string


view : Model -> Html Msg
view model =
    select [ on "change" (Json.Decode.map Fetch selectedOptionIdDecoder) ]
        (List.map
            (\opt ->
                option [ id opt.url ] [ text opt.name ]
            )
            model.options
        )


init =
    Model
        [ { name = "Select a Text...", url = "/nothig.txt" }
        , { name = "Pride and Perjudice", url = "/test.txt" }
        ]
        Nothing
        Nothing



-- This will return the text divided by words.
-- It will also convert \n to \r which is what the ENTER key produces.


getText : String -> Cmd Msg
getText file =
    Task.perform FetchFailure
        FetchSuccess
        ((Http.getString file)
            |> Task.map
                (Utils.words
                    >> List.map
                        (List.map Utils.lfToCr)
                )
        )
