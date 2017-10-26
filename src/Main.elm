module Main exposing (..)

import Char exposing (fromCode, toCode)
import Element as E
import Element.Attributes as EA
import Element.Events as EE
import Element.Input as EI
import Html exposing (Html, div, img, option, select, span, text)
import Html.Attributes exposing (property, src, style, value)
import Html.Events exposing (on, onClick, targetValue)
import Json.Decode as JD
import Set
import String exposing (fromChar, toLower)
import Style as S
import Style.Background
import Style.Border


type alias Model =
    { config :
        Result String
            { emojis : List Emoji
            , emojisUrl : String
            }
    , selectedEmoji : Maybe Emoji
    , selectedCategory : Maybe String
    }


type alias Config =
    { emojis : List Emoji
    , emojisUrl : String
    }


type alias Emoji =
    { name : Maybe String
    , x : Int
    , y : Int
    , category : String
    }


decodeEmojis : JD.Decoder (List Emoji)
decodeEmojis =
    JD.list
        (JD.map4 Emoji
            (JD.maybe (JD.field "name" JD.string))
            (JD.field "sheet_x" JD.int)
            (JD.field "sheet_y" JD.int)
            (JD.field "category" JD.string)
        )


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    let
        result =
            JD.decodeValue (JD.map2 Config (JD.index 0 decodeEmojis) (JD.index 1 JD.string)) flags
    in
    ( { config = result, selectedEmoji = Nothing, selectedCategory = Nothing }, Cmd.none )


type Msg
    = SelectEmoji String
    | SelectCategory String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectEmoji name ->
            let
                selectedEmoji =
                    Result.map
                        (\config ->
                            List.filter (\emoji -> emoji.name == Just name) config.emojis
                                |> List.head
                        )
                        model.config
                        |> Result.toMaybe
                        |> Maybe.andThen (\res -> res)
            in
            ( { model | selectedEmoji = selectedEmoji }
            , Cmd.none
            )

        SelectCategory category ->
            ( { model | selectedCategory = Just category }, Cmd.none )


hexToInt : String -> Int
hexToInt =
    String.foldl (\hexDigit int -> int * 16 + toCode hexDigit % 39 - 9) 0 << toLower


codeToStr : Int -> String
codeToStr =
    fromChar << fromCode


spriteCss : Int -> Int -> String -> List ( String, String )
spriteCss x y url =
    [ ( "width", "32px" )
    , ( "height", "32px" )
    , ( "background", "url(" ++ url ++ ")" )
    , ( "background-position", toString x ++ "px " ++ "-" ++ toString y ++ "px" )
    ]
        |> Debug.log "mhey"


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just a ->
            True

        Nothing ->
            False


viewCategorySelector : Maybe String -> List Emoji -> E.Element Styles variation Msg
viewCategorySelector activeCategory emojis =
    let
        categories =
            Set.fromList (List.map .category emojis)
                |> Set.toList

        navStyle =
            \cat ->
                if activeCategory == Just cat then
                    ActiveCategory
                else
                    None

        options =
            List.map (\cat -> E.text cat |> E.el (navStyle cat) [ EE.onClick (SelectCategory cat) ]) categories
    in
    E.navigation CategoryList
        [ EA.spacing 10 ]
        { options = options
        , name = "Category"
        }


viewEmoji : String -> Emoji -> E.Element Styles variation Msg
viewEmoji url emoji =
    E.html <|
        div ((Maybe.withDefault [] <| Maybe.map (\name -> [ onClick (SelectEmoji name) ]) emoji.name) ++ [ style <| spriteCss (32 * emoji.x) (32 * emoji.y) url ]) []


emojisByCategory : String -> List Emoji -> List Emoji
emojisByCategory category =
    List.filter (.category >> (==) category)


viewEmojis : String -> List Emoji -> E.Element Styles variation Msg
viewEmojis url emojis =
    let
        length =
            toFloat <| List.length emojis

        x =
            min 8 <| ceiling <| sqrt length

        y =
            ceiling <| (length / 8)

        columns =
            List.range 0 x |> List.map (\a -> EA.px 32)

        rows =
            List.range 0 y |> List.map (\a -> EA.px 32)

        cells =
            List.indexedMap
                (\i emoji ->
                    E.cell
                        { start = ( rem i x, i // y )
                        , width = 1
                        , height = 1
                        , content =
                            viewEmoji url emoji
                        }
                )
                emojis
    in
    E.grid None
        [ EA.yScrollbar, EA.maxHeight (EA.px 550), EA.spacing 24 ]
        { columns = columns
        , rows = rows
        , cells = cells
        }


type Styles
    = None
    | ActiveCategory
    | CategoryList
    | EmojisWidget


stylesheet =
    S.styleSheet
        [ S.style None []
        , S.style ActiveCategory [ Style.Border.bottom 2, Style.Border.solid ]
        , S.style CategoryList [ Style.Border.bottom 1, Style.Border.solid ]
        , S.style EmojisWidget [ Style.Border.all 1, Style.Border.rounded 2, Style.Border.solid ]
        ]


view : Model -> Html Msg
view model =
    Result.map
        (\config ->
            let
                selectedEmojis =
                    Maybe.map (\category -> emojisByCategory category config.emojis) model.selectedCategory
            in
            E.column EmojisWidget
                [ EA.center ]
                [ viewCategorySelector model.selectedCategory config.emojis
                , Maybe.map (viewEmojis config.emojisUrl) selectedEmojis
                    |> Maybe.withDefault (E.text "No category selected")
                , Maybe.map (viewEmoji config.emojisUrl) model.selectedEmoji
                    |> Maybe.withDefault (E.text "No emoji selected")
                ]
                |> E.layout stylesheet
        )
        model.config
        |> Result.toMaybe
        |> Maybe.withDefault (text "Loading ...")


main : Program JD.Value Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
