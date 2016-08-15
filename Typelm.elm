module Typelm exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Keyboard
import Char
import String
import Debug exposing (log)
import Task
import Http
import Time exposing (Time, second)
import Utils
import Maybe.Extra exposing (isNothing)
import TextFetch


{- Overall strategy:
   textByWords - the whole text, as List (List Char) where each inner list is a word
   userText - what the user has typed in the current page. It is backwards, so we add new chars with :: (cons)
   displayedText - the contents of the current page. it gets recalculated when the user has typed all of it

   In the view, the two displayedText and userText are compared to identify right and wrong entries.
-}
--TODO: Store last page number for each text in the localStorage, So the user can continue where they left. 
--      (The TextFetch module would provide the number along with the text)


type alias Model =
    { textByWords : List (List Char)
    , userText : List Char
    , displayedText : List Char
    , page : Int
    , wpm : Int
    , startTime : Maybe Time
    , textFetch : TextFetch.Model
    }



-- Update


type Msg
    = CharKeyMsg Keyboard.KeyCode
    | KeyMsg Keyboard.KeyCode
    | StartSucceed Time
    | StartFailed
    | Tick Time
    | TurnPage
    | TextFetchMsg TextFetch.Msg


wordsPerPage =
    20


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CharKeyMsg code ->
            ( { model | userText = (Char.fromCode code) :: model.userText }
            , if isNothing model.startTime then
                Task.perform (\_ -> StartFailed) StartSucceed Time.now
              else if List.length model.userText == (List.length model.displayedText) then
                -- The user has typed all the text on screen.
                -- We need to wrap the call to update in a task to run it as a command.
                Task.perform (\_ -> Debug.crash "This failure cannot happen.") identity (Task.succeed TurnPage)
              else
                Cmd.none
            )

        KeyMsg code ->
            if code == 8 then
                -- code 8 corresponds to backspace.
                -- We remove the first item from the list, which is the last in the text, because userText is reversed
                ( { model | userText = model.userText |> List.tail |> Maybe.withDefault [] }, Cmd.none )
            else
                ( model, Cmd.none )

        TurnPage ->
            ( { model
                | page = model.page + 1
                , displayedText =
                    model.textByWords
                        |> List.drop (model.page * wordsPerPage)
                        |> List.take wordsPerPage
                        |> List.intersperse [ ' ' ]
                        |> List.concat
                , userText = []
                , startTime = Nothing
              }
            , Cmd.none
            )

        StartSucceed time ->
            ( { model | startTime = Just time }, Cmd.none )

        StartFailed ->
            ( model, Cmd.none )

        Tick currentTime ->
            case model.startTime of
                Nothing ->
                    ( model, Cmd.none )

                Just startTime ->
                    ( { model | wpm = calculateWpm (List.reverse model.userText) model.displayedText startTime currentTime }
                    , Cmd.none
                    )

        TextFetchMsg childMsg ->
            let
                ( newChildState, childCmd, selectedText ) =
                    TextFetch.update childMsg model.textFetch

                newModel =
                    case selectedText of
                        Just newText ->
                            { model
                                | textByWords = newText
                                , userText = []
                                , displayedText =
                                    newText
                                        |> List.take wordsPerPage
                                        |> List.intersperse [ ' ' ]
                                        |> List.concat
                                , wpm = 0
                                , page = 1
                                , startTime = Nothing
                            }

                        Nothing ->
                            model
            in
                ( { newModel | textFetch = newChildState }
                , Cmd.map TextFetchMsg childCmd
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
                    --space
                    [ text (String.fromChar '␣'), wbr [] [] ]

                13 ->
                    --enter
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
                model.displayedText
                (List.reverse model.userText)
            )

        typedLength =
            List.length typed

        untyped =
            model.displayedText
                |> List.drop typedLength
                |> List.map
                    (\x ->
                        Neutral x
                    )
    in
        div [ class "typelm-container" ]
            [ div [ class "typelm-header" ]
                [ (text (toString model.wpm ++ " wpm"))
                , (App.map TextFetchMsg (TextFetch.view model.textFetch))
                ]
            , div [ class "typelm-text" ]
                (List.indexedMap
                    (\i x -> charachterView (i == typedLength) x)
                    (typed ++ untyped)
                )
            ]



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.presses CharKeyMsg
        , Keyboard.downs KeyMsg
        , Time.every second Tick
        ]



-- Init


init : ( Model, Cmd Msg )
init =
    ( { textByWords = Utils.words "Hello World!"
      , userText = String.toList ""
      , displayedText = String.toList "Hello World!"
      , page = 0
      , wpm = 0
      , startTime = Nothing
      , textFetch = TextFetch.init
      }
    , Cmd.none
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
