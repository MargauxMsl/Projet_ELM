module GuessWord exposing (..)

import Browser
import Http
import Html exposing (..)
import Html exposing (Html, Attribute, button, div, form, h1, input, text)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Html.Events exposing (onInput)
import Html exposing (text)
import Html.Events exposing (onClick)
import String
import Array
import Random
import Json.Decode exposing (..)

-- TYPE DEFINITIONS

type State
    = Failure
    | Loading
    | GotWords
    | Success String

type WinOrLose 
    = Win
    | Lose
    | Unknown

type alias Model =
    { state : State
    , url : String
    , listOfWords : List String
    , guess : String
    , word : String
    , index : Int
    , dictionary : List WordInfo
    , winOrLose : WinOrLose
    }

type alias WordInfo = 
    { word : String
    , meanings : List Dictionary
    }

type alias Dictionary =
    { partOfSpeech : String
    , definitions : List Def
    }

type alias Def =
    { definition : String
    }

type Msg
    = Words (Result Http.Error String)
    | RandomNumber Int
    --| RandomWord String
    | GotDictionnary (Result Http.Error (List WordInfo))
    | NewGuess String
    | CheckAnswer

-- MAIN
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- INITIALIZATION
init : () -> (Model, Cmd Msg)
init _ =
    ( Model Loading "" [] "" "" 0 [] Unknown
    , Http.get
        { url = "./Data.txt"
        , expect = Http.expectString Words
        }
    )

--UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Words result ->
            case result of
                Ok words ->
                    ({model | listOfWords = String.words words
                    , state = GotWords
                    }
                    , getRandomNumber model
                    )

                Err _ ->
                    ({model | state = Failure}
                    , Cmd.none
                    )

        RandomNumber result ->
            case result of
                x ->
                    ({model | index = x, word = getRandWord model.listOfWords x, url = ("https://api.dictionaryapi.dev/api/v2/entries/en/" ++ (getRandWord model.listOfWords x)), state = Success "got random word"}
                    , getDictionary model
                    )
                --Nothing ->
                 --   (model, Cmd.none)
            
        
        {--RandomWord word ->
            ({model | word = word}
            , getDictionary word
            )      --}              
        
        GotDictionnary result ->
            case result of
                Ok definitions ->
                    ({model | state=Success "Got dictionnary"
                    , dictionary = definitions}
                    , Cmd.none
                    )
                Err _ -> 
                    ({model | dictionary = []}
                    , Cmd.none
                    )
        
        NewGuess guess ->
            ({model| guess = guess}
            , Cmd.none
            )
        
        CheckAnswer ->
            if (String.toLower model.guess) == (String.toLower model.word) then
                ({model | state = Success "Correct!", winOrLose = Win}
                , Cmd.none
                )
            else
                ({model | state = Success "Incorrect", winOrLose = Lose}
                , Cmd.none
                )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none

-- VIEW
view : Model -> Html Msg
view model =
    case model.state of
        Success state -> 
            case state of
            "" ->
                div []
                    [ h1 [] [ text "Guess the W _ _ _ ! "] 
                    , div [style "text-align" "center"]
                        [div [] (List.map viewWordInfo model.dictionary), text (model.word ++ model.url)]
                    , div []
                        [ input [Html.Attributes.placeholder "Who am I ?", Html.Attributes.value model.guess, onInput NewGuess] []
                        , button [onClick CheckAnswer] [text "Submit"]
                        ]
                    ]
            "Correct!" ->
                div []
                    [ checkAnswer model ]
            "Incorrect" ->
                div []
                    [checkAnswer model]
            "got random word" ->
                div[]
                    [text (model.word ++ model.url)]
            _ ->
                div[][]
            
        
        Failure -> 
            text "Erreur lors du chargement"

        Loading ->
            text "En cours de chargement"
        
        GotWords ->
            text "got data"

viewWordInfo : WordInfo -> Html Msg
viewWordInfo info = 
    div []
        [ h2[] (List.map viewDefinitions info.meanings)
        ]

viewDefinitions : Dictionary -> Html Msg
viewDefinitions dictionary = 
    div []
        [ li [] [text dictionary.partOfSpeech]
        , p [] (List.map viewDef dictionary.definitions)
        ]

viewDef : Def -> Html Msg
viewDef def =
    text def.definition


-- FUNCTIONS
getRandomNumber : Model -> Cmd Msg
getRandomNumber model =
    Random.generate RandomNumber (Random.int 0 999)

{--getRandomNumber : Model -> Cmd Msg
getRandomNumber model = Random.generate RandomNumber (Random.map (Maybe.withDefault 0 Just(Random.int 0 ((Array.length (Array.fromList model.listOfWords))-1))))
--}

getRandWord : List String -> Int -> String
getRandWord wordList index = Maybe.withDefault "" (Array.get index (Array.fromList wordList))

getDictionary : Model -> Cmd Msg
getDictionary model = 
    Http.get 
        {url = ("https://api.dictionaryapi.dev/api/v2/entries/en/" ++ model.word)
        , expect = Http.expectJson GotDictionnary listDicoDecoder
        }

listDicoDecoder : Decoder (List WordInfo)
listDicoDecoder = Json.Decode.list wordInfoDecoder

wordInfoDecoder : Decoder WordInfo
wordInfoDecoder = 
    map2 WordInfo 
        (field "word" string)
        (field "meanings" <|Json.Decode.list dicoDecoder)

dicoDecoder : Decoder Dictionary
dicoDecoder = 
    Json.Decode.map2 Dictionary 
        (field "partOfSpeech" string)
        (field "definitions" <|Json.Decode.list defDecoder)

defDecoder : Decoder Def
defDecoder = 
    Json.Decode.map Def
        (field "definition" string)

checkAnswer : Model -> Html Msg
checkAnswer model = 
    if model.guess == model.word then
        div rightAnswerStyle
            [text "Congratulations ! \n Wanna play again ?"
            , div []
                [button ([Html.Attributes.type_ "Yes"]  ++ yesButtonStyle) [text "Yes"]]
            , div []
                [button ([Html.Attributes.type_ "No"] ++ noButtonStyle) [text "No"]]
            ]
    else if model.guess == "" then
        div wrongAnswerStyle
        [text "Wrong answer ! \n Wanna try again ?"
            , div [] 
                [button ([Html.Attributes.type_ "Yes"]  ++ yesButtonStyle)
                    [text "Yes"]
                ]   
            , div []
                [button ([Html.Attributes.type_ "No"] ++ noButtonStyle)
                    [text "No, new word"]
                ]
        ]
    else
        view model

headerStyle : List (Attribute msg)
headerStyle = 
    [ style "text-align" "center"
    , style "top-margin" "80px"
    , style "fontSize" "80px"
    , style "fontWeight" "Light"
    , style "color" "white"
    , style "font-family" "Bodoni MT Condensed"
    ]

backFormStyle : List (Attribute msg)
backFormStyle =
    [ style "border-radius" "5px"
    , style "background-color" "rgb(242, 104, 36)"
    , style "text-align" "center"
    , style "width" "fill"
    , style "margin" "auto"
    , style "position" "absolute"
    , style "top" "50%"
    , style "left" "50%"
    , style "transform" "translate(-50%, -50%)"
    ]

formStyle : List (Attribute msg)
formStyle =
    [ style "border-radius" "5px"
    , style "background-color" "rgb(239, 187, 167)"
    , style "text-align" "center"
    , style "width" "300px"
    , style "margin" "auto"
    , style "position" "absolute"
    , style "top" "50%"
    , style "left" "50%"
    , style "transform" "translate(-50%, -50%)"
    ]

rightAnswerStyle : List (Attribute msg)
rightAnswerStyle = [ style "border-radius" "5px"
    , style "background-color" "rgb(162, 217, 152)"
    , style "text-align" "center"
    , style "width" "300px"
    , style "margin" "auto"
    , style "position" "absolute"
    , style "top" "50%"
    , style "left" "50%"
    , style "transform" "translate(-50%, -50%)"
    ]

wrongAnswerStyle : List (Attribute msg)
wrongAnswerStyle = [ style "border-radius" "5px"
    , style "background-color" "rgb(162, 217, 152)"
    , style "text-align" "center"
    , style "width" "300px"
    , style "margin" "auto"
    , style "position" "absolute"
    , style "top" "50%"
    , style "left" "50%"
    , style "transform" "translate(-50%, -50%)"
    ]


yesButtonStyle : List (Attribute msg)
yesButtonStyle = 
    [ style "width" "50px"
    , style "height" "50px"
    , style "border-radius" "15"
    , style "background-color" "#397cd5"
    , style "color" "white"
    , style "padding" "14px 20px"
    , style "margin-top" "10px"
    , style "border" "none"
    , style "border-radius" "4px"
    , style "font-size" "16px"
    ]

noButtonStyle : List (Attribute msg)
noButtonStyle = 
    [ style "width" "50px"
    , style "height" "50px"
    , style "border-radius" "15"
    , style "background-color" "red"
    , style "color" "white"
    , style "padding" "14px 20px"
    , style "margin-top" "10px"
    , style "border" "none"
    , style "border-radius" "4px"
    , style "font-size" "16px"
    ]