module Typelm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Keyboard
import Char
import String
import Debug exposing (log)
import Maybe
import Task
import Http
import Time exposing (Time, second)
import Utils
import Maybe.Extra exposing (isNothing)


type alias Model =
    { text : List Char
    , userText : List Char
    , wpm : Int
    , startTime : Maybe Time
    }



-- Update


type Msg
    = CharKeyMsg Keyboard.KeyCode
    | KeyMsg Keyboard.KeyCode
    | FetchFail Http.Error
    | FetchSucceed (List Char)
    | StartSucceed Time
    | StartFailed
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CharKeyMsg code ->
            ( { model | userText = (Char.fromCode code) :: model.userText }
            , if isNothing model.startTime then
                Task.perform (\_ -> StartFailed) StartSucceed Time.now
              else
                Cmd.none
            )

        KeyMsg code ->
            (if code == 8 then
                -- code 8 corresponds to backspace
                ( { model | userText = model.userText |> List.tail |> Maybe.withDefault [] }, Cmd.none )
             else
                ( model, Cmd.none )
            )

        FetchFail error ->
            ( model, Cmd.none )

        -- Load the new text. we connvert code 10 to 13 which corresponds to the ENTER key
        FetchSucceed textList ->
            ( Model textList [] 0 Nothing, Cmd.none )

        StartSucceed time ->
            ( { model | startTime = Just time }, Cmd.none )

        StartFailed ->
            ( model, Cmd.none )

        Tick currentTime ->
            case model.startTime of
                Nothing ->
                    ( model, Cmd.none )

                Just startTime ->
                    ( { model | wpm = calculateWpm (List.reverse model.userText) model.text startTime currentTime }
                    , Cmd.none
                    )


calculateWpm : List Char -> List Char -> Time -> Time -> Int
calculateWpm listA listB startTime currentTime =
    let
        ( correct, incorrect ) =
            Utils.foldl2
                (\a b ( c, i ) ->
                    if a == b then
                        ( c + 1, i )
                    else
                        ( c, i + 1 )
                )
                ( 0, 0 )
                listA
                listB

        elapsedSeconds =
            (Time.inSeconds currentTime) - (Time.inSeconds startTime)
    in
        (((correct + incorrect) / 5) - incorrect)
            / (elapsedSeconds / 60)
            |> round
            |> Utils.atLeast 0



-- View


type Character
    = Neutral Char
    | Correct Char
    | Incorrect Char


charachterView : Bool -> Character -> Html Msg
charachterView isNext character =
    let
        ( className, char ) =
            case character of
                Correct c ->
                    ( "correct", c )

                Incorrect c ->
                    ( "incorrect", c )

                Neutral c ->
                    ( "neutral", c )

        children =
            case (Char.toCode char) of
                32 ->
                    [ text (String.fromChar '␣'), wbr [] [] ]

                13 ->
                    [ text (String.fromChar '↵'), br [] [] ]

                _ ->
                    [ text (String.fromChar char) ]
    in
        span [ classList [ ( className, True ), ( "next", isNext ) ] ]
            children


view : Model -> Html Msg
view model =
    let
        typed =
            (List.map2
                (\a b ->
                    if a == b then
                        Correct a
                    else
                        Incorrect a
                )
                model.text
                (List.reverse model.userText)
            )

        typedLength =
            List.length typed

        untyped =
            model.text
                |> List.drop typedLength
                |> List.map
                    (\x ->
                        Neutral x
                    )
    in
        div []
            [ div []
                (List.indexedMap
                    (\i x -> charachterView (i == typedLength) x)
                    (typed ++ untyped)
                )
            , text <| toString <| model.wpm
            ]



-- Init


init : ( Model, Cmd Msg )
init =
    ( { text = String.toList "Hola como Estas"
      , userText = String.toList ""
      , wpm = 0
      , startTime = Nothing
      }
    , getText "test.txt"
    )



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses CharKeyMsg
        , Keyboard.downs KeyMsg
        , Time.every second Tick
        ]



-- Http


getText : String -> Cmd Msg
getText file =
    Task.perform FetchFail
        FetchSucceed
        ((Http.getString file)
            |> Task.map
                (String.toList
                    >> List.map
                        (\x ->
                            if Char.toCode x == 10 then
                                Char.fromCode 13
                            else
                                x
                        )
                )
        )



-- Main


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
