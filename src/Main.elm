module Main exposing (..)

import Char exposing (fromCode, toCode)
import Color
import Colorbrewer.Qualitative
import Element as E
import Element.Attributes as EA
import Element.Events as EE
import Element.Input as EI
import Html exposing (Html, div, img, option, select, span, text)
import Html.Lazy exposing (lazy)
import Json.Decode as JD
import Keyboard
import List.Extra
import Platform.Sub
import Set
import String exposing (fromChar, toLower)
import Style as S
import Style.Border
import Style.Color
import Style.Font


type alias Model =
    { emojis : List Emoji
    , emojisUrl : String
    , categories : List String
    , selectedEmoji : Maybe Emoji
    , selectedCategory : Maybe String
    , searchString : Maybe String
    }


type alias Emoji =
    { name : String
    , x : Int
    , y : Int
    , category : String
    , shortNames : List String
    , sortOrder : Float
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
            (JD.map6 Emoji
                (JD.field "name" JD.string)
                (JD.field "sheet_x" JD.int)
                (JD.field "sheet_y" JD.int)
                (JD.field "category" JD.string)
                (JD.field "short_names" (JD.list JD.string))
                (JD.field "sort_order" JD.float)
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
                |> List.filter ((/=) "Skin Tones")
    in
    ( { emojis = emojis
      , emojisUrl = emojisUrl
      , categories = categories
      , selectedEmoji = Nothing
      , selectedCategory = Just "People"
      , searchString = Nothing
      }
    , Cmd.none
    )


type Msg
    = Noop
    | SelectEmoji String
    | SelectCategory String
    | CursorMove Cursor
    | SearchInput String


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

        SearchInput string ->
            ( { model
                | searchString =
                    if String.length string > 0 then
                        Just string
                    else
                        Nothing
              }
            , Cmd.none
            )


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
                    InactiveCategory

        options =
            List.map (\cat -> E.text cat |> E.el (navStyle cat) [ EA.paddingBottom 4, EE.onClick (SelectCategory cat) ]) categories
    in
    E.navigation CategoryList
        [ EA.spacing 8, EA.width EA.fill, EA.padding 8 ]
        { options = options
        , name = "Category"
        }


viewSearchFilter : String -> E.Element Styles variation Msg
viewSearchFilter searchString =
    E.el None [ EA.padding 5, EA.width EA.fill ] <|
        EI.search SearchFilter
            [ EA.padding 5 ]
            { onChange = SearchInput
            , value = searchString
            , label = EI.placeholder { text = "Search", label = EI.hiddenLabel "Search" }
            , options = []
            }


emojiStyle : Int -> Styles
emojiStyle index =
    case index % 5 of
        0 ->
            EmojiItem First

        1 ->
            EmojiItem Second

        2 ->
            EmojiItem Third

        3 ->
            EmojiItem Forth

        4 ->
            EmojiItem Fifth

        _ ->
            EmojiItem First


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
                , E.el EmojiDetailText [] (E.text em.name)
                ]
            )
            emoji
            |> Maybe.withDefault [ E.text "No emoji selected" |> E.el EmojiDetailText [] ]
        )


emojisByCategory : String -> List Emoji -> List Emoji
emojisByCategory category =
    List.filter (.category >> (==) category)


emojisFilteredByString : String -> List Emoji -> List Emoji
emojisFilteredByString string =
    List.filter (.name >> String.contains string)


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
    | InactiveCategory
    | CategoryList
    | EmojisWidget
    | EmojisGrid
    | EmojiItem BackgroundStyle
    | EmojiDetailText
    | SearchFilter


type BackgroundStyle
    = First
    | Second
    | Third
    | Forth
    | Fifth


stylesheet : S.StyleSheet Styles variation
stylesheet =
    let
        fonts =
            Style.Font.typeface
                [ Style.Font.font "Helvetica"
                , Style.Font.font "Trebuchet MS"
                , Style.Font.font "Verdana"
                , Style.Font.font "sans-serif"
                ]
    in
    S.styleSheet
        [ S.style None []
        , S.style ActiveCategory [ Style.Border.bottom 2, Style.Border.solid, fonts ]
        , S.style InactiveCategory [ fonts ]
        , S.style CategoryList []
        , S.style EmojisWidget
            [ Style.Border.all 1
            , Style.Border.rounded 6
            , Style.Border.solid
            , Style.Color.border (Color.rgba 0 0 0 0.15)
            , Style.Color.background (Color.rgb 249 249 249)
            ]
        , S.style EmojisGrid [ Style.Color.background Color.white ]
        , S.style (EmojiItem First)
            [ S.hover
                [ Style.Border.rounded 10
                , Style.Color.background Colorbrewer.Qualitative.pastel15_0
                ]
            ]
        , S.style (EmojiItem Second)
            [ S.hover
                [ Style.Border.rounded 10
                , Style.Color.background Colorbrewer.Qualitative.pastel15_1
                ]
            ]
        , S.style (EmojiItem Third)
            [ S.hover
                [ Style.Border.rounded 10
                , Style.Color.background Colorbrewer.Qualitative.pastel15_2
                ]
            ]
        , S.style (EmojiItem Forth)
            [ S.hover
                [ Style.Border.rounded 10
                , Style.Color.background Colorbrewer.Qualitative.pastel15_3
                ]
            ]
        , S.style (EmojiItem Fifth)
            [ S.hover
                [ Style.Border.rounded 10
                , Style.Color.background Colorbrewer.Qualitative.pastel15_4
                ]
            ]
        , S.style EmojiDetailText
            [ fonts ]
        , S.style SearchFilter
            [ fonts
            , Style.Font.size 13
            , Style.Border.all 1
            , Style.Border.rounded 6
            , Style.Color.border (Color.rgba 0 0 0 0.15)
            , Style.Color.background (Color.rgb 249 249 249)
            ]
        ]


view : Model -> Html Msg
view model =
    let
        hasSearchString =
            isJust model.searchString

        selectedEmojis =
            (if not hasSearchString then
                Maybe.map (\category -> emojisByCategory category model.emojis) model.selectedCategory
             else
                Maybe.map (\string -> emojisFilteredByString string model.emojis) model.searchString
            )
                |> Maybe.map (List.sortBy .sortOrder)
    in
    E.column EmojisWidget
        [ EA.center ]
        [ viewCategorySelector model.selectedCategory model.categories
        , viewSearchFilter (Maybe.withDefault "" model.searchString)
        , Maybe.map (viewEmojis model.emojisUrl) selectedEmojis
            |> Maybe.withDefault (E.text "No category selected" |> E.el EmojiDetailText [])
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
