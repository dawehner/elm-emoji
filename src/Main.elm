module Main exposing (..)

import Char exposing (fromCode, toCode)
import Color
import Element as E
import Element.Attributes as EA
import Element.Events as EE
import Html exposing (Html, div, img, option, select, span, text)
import Json.Decode as JD
import Keyboard
import List.Extra
import Platform.Sub
import Set
import String exposing (fromChar, toLower)
import Style as S
import Style.Border
import Style.Color


type alias Model =
    { emojis : List Emoji
    , emojisUrl : String
    , categories : List String
    , selectedEmoji : Maybe Emoji
    , selectedCategory : Maybe String
    }


type alias Emoji =
    { name : String
    , x : Int
    , y : Int
    , category : String
    }


type Cursor
    = Left
    | Up
    | Right
    | Down


type Selection
    = NoSelection
    | CategorySelection String
    | EmojiSelection String


decodeEmojis : JD.Decoder (List Emoji)
decodeEmojis =
    JD.list
        (JD.maybe
            (JD.map4 Emoji
                (JD.field "name" JD.string)
                (JD.field "sheet_x" JD.int)
                (JD.field "sheet_y" JD.int)
                (JD.field "category" JD.string)
            )
        )
        |> JD.map (List.filterMap identity)


init : JD.Value -> ( Model, Cmd Msg )
init flags =
    let
        emojis =
            JD.decodeValue (JD.index 0 decodeEmojis) flags
                |> Result.withDefault []

        emojisUrl =
            JD.decodeValue (JD.index 1 JD.string) flags
                |> Result.withDefault ""

        categories =
            List.map .category emojis
                |> Set.fromList
                |> Set.toList
    in
    ( { emojis = emojis
      , emojisUrl = emojisUrl
      , categories = categories
      , selectedEmoji = Nothing
      , selectedCategory = Just "People"
      }
    , Cmd.none
    )


type Msg
    = Noop
    | SelectEmoji String
    | SelectCategory String
    | CursorMove Cursor


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        SelectEmoji name ->
            let
                selectedEmoji =
                    List.filter (.name >> (==) name) model.emojis
                        |> List.head
            in
            ( { model | selectedEmoji = selectedEmoji }
            , Cmd.none
            )

        SelectCategory category ->
            ( { model | selectedCategory = Just category }, Cmd.none )

        CursorMove cursor ->
            case cursor of
                Left ->
                    case model.selectedCategory of
                        Just category ->
                            let
                                selectedCategory =
                                    List.Extra.elemIndex category model.categories
                                        |> Maybe.map (\i -> i - 1)
                                        |> Maybe.map (flip (%) (List.length model.categories))
                                        |> Maybe.andThen (\i -> List.Extra.getAt i model.categories)
                            in
                            ( { model | selectedCategory = selectedCategory }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Right ->
                    case model.selectedCategory of
                        Just category ->
                            let
                                selectedCategory =
                                    List.Extra.elemIndex category model.categories
                                        |> Maybe.map (\i -> i + 1)
                                        |> Maybe.map (flip (%) (List.length model.categories))
                                        |> Maybe.andThen (\i -> List.Extra.getAt i model.categories)
                            in
                            ( { model | selectedCategory = selectedCategory }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Up ->
                    ( model, Cmd.none )

                Down ->
                    ( model, Cmd.none )


hexToInt : String -> Int
hexToInt =
    String.foldl (\hexDigit int -> int * 16 + toCode hexDigit % 39 - 9) 0 << toLower


codeToStr : Int -> String
codeToStr =
    fromChar << fromCode


spriteCss : Int -> Int -> String -> List ( String, String )
spriteCss x y url =
    [ ( "background", "url(" ++ url ++ ")" )
    , ( "background-position", "-" ++ toString x ++ "px " ++ "-" ++ toString y ++ "px" )
    ]


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just a ->
            True

        Nothing ->
            False


viewCategorySelector : Maybe String -> List String -> E.Element Styles variation Msg
viewCategorySelector activeCategory categories =
    let
        navStyle =
            \cat ->
                if activeCategory == Just cat then
                    ActiveCategory
                else
                    None

        options =
            List.map (\cat -> E.text cat |> E.el (navStyle cat) [ EA.paddingBottom 4, EE.onClick (SelectCategory cat) ]) categories
    in
    E.navigation CategoryList
        [ EA.spacing 8, EA.width (EA.percent 100), EA.padding 8 ]
        { options = options
        , name = "Category"
        }


emojiStyle : Int -> Styles
emojiStyle index =
    case index % 2 of
        0 ->
            EmojiItem Green

        1 ->
            EmojiItem Gray

        _ ->
            EmojiItem Green


viewEmoji : Int -> String -> Emoji -> E.Element Styles variation Msg
viewEmoji index url emoji =
    E.el (emojiStyle index)
        [ EE.onClick (SelectEmoji emoji.name)
        , EA.height (EA.px 64)
        , EA.width (EA.px 64)
        , EA.center
        ]
    <|
        E.el None
            [ EA.center
            , EA.verticalCenter
            , EA.inlineStyle <| spriteCss (32 * emoji.x) (32 * emoji.y) url
            , EA.height (EA.px 32)
            , EA.width (EA.px 32)
            ]
            E.empty


viewEmojiDetail : String -> Maybe Emoji -> E.Element Styles variation Msg
viewEmojiDetail url emoji =
    E.row None
        [ EA.padding 8, EA.center, EA.verticalCenter, EA.spacing 8 ]
        (Maybe.map
            (\em ->
                [ E.el None
                    [ spriteCss (32 * em.x) (32 * em.y) url |> EA.inlineStyle
                    , EA.height (EA.px 32)
                    , EA.width (EA.px 32)
                    ]
                    E.empty
                , E.text em.name
                ]
            )
            emoji
            |> Maybe.withDefault [ E.text "No emoji selected" ]
        )


emojisByCategory : String -> List Emoji -> List Emoji
emojisByCategory category emojis =
    List.filter (.category >> (==) category) emojis


viewEmojis : String -> List Emoji -> E.Element Styles variation Msg
viewEmojis url emojis =
    let
        length =
            List.length emojis

        x =
            9

        y =
            length // x

        columns =
            List.range 0 (x - 1) |> List.map (\a -> EA.px 64)

        rows =
            List.range 0 (y - 1) |> List.map (\a -> EA.px 64)

        cells =
            List.indexedMap
                (\i emoji ->
                    E.cell
                        { start = ( rem i x, i // x )
                        , width = 1
                        , height = 1
                        , content =
                            viewEmoji i url emoji
                        }
                )
                emojis
    in
    E.grid EmojisGrid
        [ EA.yScrollbar, EA.maxHeight (EA.px 300) ]
        { columns = columns
        , rows = rows
        , cells = cells
        }


type Styles
    = None
    | ActiveCategory
    | CategoryList
    | EmojisWidget
    | EmojisGrid
    | EmojiItem BackgroundStyle


type BackgroundStyle
    = Green
    | Gray


stylesheet : S.StyleSheet Styles variation
stylesheet =
    S.styleSheet
        [ S.style None []
        , S.style ActiveCategory [ Style.Border.bottom 2, Style.Border.solid ]
        , S.style CategoryList []
        , S.style EmojisWidget
            [ Style.Border.all 1
            , Style.Border.rounded 6
            , Style.Border.solid
            , Style.Color.border (Color.rgba 0 0 0 0.15)
            , Style.Color.background (Color.rgb 249 249 249)
            ]
        , S.style EmojisGrid [ Style.Color.background Color.white ]
        , S.style (EmojiItem Green)
            [ S.hover
                [ Style.Color.background Color.green
                ]
            ]
        , S.style (EmojiItem Gray)
            [ S.hover
                [ Style.Color.background Color.gray
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        selectedEmojis =
            Maybe.map (\category -> emojisByCategory category model.emojis) model.selectedCategory
    in
    E.column EmojisWidget
        [ EA.center ]
        [ viewCategorySelector model.selectedCategory model.categories
        , Maybe.map (viewEmojis model.emojisUrl) selectedEmojis
            |> Maybe.withDefault (E.text "No category selected")
        , viewEmojiDetail model.emojisUrl model.selectedEmoji
        ]
        |> E.layout stylesheet


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs
        (\code ->
            let
                left =
                    37

                up =
                    38

                right =
                    39

                down =
                    40
            in
            case code of
                37 ->
                    CursorMove Left

                39 ->
                    CursorMove Right

                38 ->
                    CursorMove Up

                40 ->
                    CursorMove Down

                _ ->
                    Noop
        )


main : Program JD.Value Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
