port module TextFetch exposing (..)

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


--TODO: Store last page number for each text in the localStorage, So the user can continue where they left.


type alias Model =
    { options : List Option
    , selectedOption : Maybe Option
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
    = Fetch Option
    | FetchSuccess Text
    | FetchFailure Http.Error
    | TurnPage
    | FetchPageSuccess Int


wordsPerPage =
    20


port urls : String -> Cmd msg


port pageSetter : ( String, Int ) -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg, Maybe (List Char) )
update msg model =
    case msg of
        Fetch option ->
            ( { model | selectedOption = Just option }, getText option.url, Nothing )

        FetchSuccess text ->
            ( { model | selectedText = (Just (log "t" text)) }
            , urls (Maybe.withDefault (Option " " " ") model.selectedOption).url
            , Nothing
            )

        FetchPageSuccess page ->
            ( { model | page = (Just (page - 1)) }
            , Task.perform (\_ -> Debug.crash "This failure cannot happen.") identity (Task.succeed TurnPage)
            , Nothing
            )

        FetchFailure _ ->
            ( model, Cmd.none, Nothing )

        TurnPage ->
            case ( model.selectedOption, model.selectedText, model.page ) of
                ( Just option, Just text, Just page ) ->
                    ( { model | page = Just (page + 1) }
                    , pageSetter ( option.url, (page + 1) )
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

                ( _, _, _ ) ->
                    ( model, Cmd.none, Nothing )


selectedOptionDecoder =
    Json.Decode.object2
        Option
        (at [ "target", "selectedOptions", "0", "id" ] string)
        (at [ "target", "selectedOptions", "0", "value" ] string)


view : Model -> Html Msg
view model =
    select [ on "change" (Json.Decode.map Fetch selectedOptionDecoder) ]
        (List.map
            (\opt ->
                option [ id opt.url, value opt.name ] [ text opt.name ]
            )
            model.options
        )


init =
    Model
        [ { name = "Select a Text...", url = "/nothig.txt" }
        , { name = "Pride and Perjudice", url = "/books/pride.txt" }
        , { name = "The Picture of Dorian Gray", url = "/books/dorian.txt" }
        ]
        Nothing
        Nothing
        Nothing



-- SUBSCRIPTIONS


subscriptions : ((Int -> Msg) -> Sub Msg) -> Model -> Sub Msg
subscriptions pageGetter model =
    pageGetter FetchPageSuccess



-- HTTP


getText : String -> Cmd Msg
getText file =
    Task.perform FetchFailure
        FetchSuccess
        ((Http.getString file)
            |> Task.map
                (Utils.dropHeaders >> Utils.removeExtraNewLines >> Utils.words)
        )
