module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Char exposing (fromCode, toCode)
import Color
import Colorbrewer.Qualitative as CQ
import Element as E
import Element.Background as EBa
import Element.Border as EB
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
import Element.Region as ER
import Html exposing (Html)
import Html.Attributes
import Html.Lazy exposing (lazy)
import Http
import Json.Decode as JD
import Platform.Sub
import Set
import String exposing (fromChar, toLower)
import Task


type alias Model =
    { emojis : List Emoji
    , emojisUrl : String
    , categories : List String
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
    , isSelected : Bool
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


type alias ScrollEvent =
    { scrollHeight : Int
    , scrollTop : Int
    , clientHeight : Int
    }


decodeEmojis : JD.Decoder (List Emoji)
decodeEmojis =
    JD.list
        (JD.maybe
            (JD.map7 Emoji
                (JD.field "name" JD.string)
                (JD.field "sheet_x" JD.int)
                (JD.field "sheet_y" JD.int)
                (JD.field "category" JD.string)
                (JD.field "short_names" (JD.list JD.string))
                (JD.field "sort_order" JD.float)
                (JD.succeed False)
            )
        )
        |> JD.map (List.filterMap identity)


fetchEmojis : String -> Cmd Msg
fetchEmojis emojiUrl =
    Http.get emojiUrl decodeEmojis
        |> Http.send FetchedEmojis


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
      , emojisUrl = "/node_modules/emoji-datasource/img/emojione/sheets/32.png"
      , categories = categories
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
    | ScrolledMessage ScrollEvent
    | FetchedEmojis (Result Http.Error (List Emoji))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        SelectEmoji name ->
            let
                emojis =
                    List.map
                        (\emoji ->
                            if emoji.name == name then
                                { emoji | isSelected = True }

                            else
                                { emoji | isSelected = False }
                        )
                        model.emojis
            in
            ( { model | emojis = emojis }
            , Cmd.none
            )

        SelectCategory category ->
            ( { model | selectedCategory = Just category }
            , Browser.Dom.setViewportOf "emojis" 0 0 |> Task.attempt (\_ -> Noop)
            )

        CursorMove cursor ->
            case cursor of
                Left ->
                    case model.selectedCategory of
                        Just category ->
                            let
                                selectedCategory =
                                    elemIndex category model.categories
                                        |> Maybe.map (\i -> i - 1)
                                        |> Maybe.map (\i -> modBy (List.length model.categories) i)
                                        |> Maybe.andThen (\i -> getAt i model.categories)
                            in
                            ( { model | selectedCategory = selectedCategory }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Right ->
                    case model.selectedCategory of
                        Just category ->
                            let
                                selectedCategory =
                                    elemIndex category model.categories
                                        |> Maybe.map (\i -> i + 1)
                                        |> Maybe.map (\i -> modBy (List.length model.categories) i)
                                        |> Maybe.andThen (\i -> getAt i model.categories)
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

        ScrolledMessage scrollEvent ->
            let
                end =
                    scrollEvent.scrollTop
                        == (scrollEvent.scrollHeight
                                - scrollEvent.clientHeight
                           )
            in
            if end then
                ( { model | selectedCategory = Maybe.andThen (\x -> cycle x model.categories) model.selectedCategory }
                , Browser.Dom.setViewportOf "emojis" 0 0 |> Task.attempt (\_ -> Noop)
                )

            else
                ( model, Cmd.none )

        FetchedEmojis emojis ->
            let
                categories =
                    Result.map
                        (\emojis_ ->
                            List.map .category emojis_
                                |> Set.fromList
                                |> Set.toList
                                |> List.filter ((/=) "Skin Tones")
                        )
                        emojis
            in
            ( { model
                | emojis = Result.withDefault [] emojis
                , categories = Result.withDefault [] categories
              }
            , Cmd.none
            )


hexToInt : String -> Int
hexToInt =
    String.foldl (\hexDigit int -> int * 16 + modBy 39 (toCode hexDigit) - 9) 0 << toLower


elemIndex : a -> List a -> Maybe Int
elemIndex x =
    findIndex ((==) x)


findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs


cycle : a -> List a -> Maybe a
cycle x xs =
    elemIndex x xs
        |> Maybe.map ((+) 1)
        |> Maybe.andThen (\x_ -> getAt x_ xs)


codeToStr : Int -> String
codeToStr =
    fromChar << fromCode


spriteCssAttribute x y url =
    [ Html.Attributes.style "background" ("url(" ++ url ++ ")")
    , Html.Attributes.style "background-position" ("-" ++ String.fromInt x ++ "px " ++ "-" ++ String.fromInt y ++ "px")
    ]


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just a ->
            True

        Nothing ->
            False


viewCategorySelector : Maybe String -> List String -> E.Element Msg
viewCategorySelector activeCategory categories =
    let
        navStyle =
            \cat ->
                if activeCategory == Just cat then
                    [ EB.widthEach
                        { top = 0
                        , left = 0
                        , right = 0
                        , bottom = 2
                        }
                    , EB.solid
                    ]

                else
                    []

        options =
            List.map
                (\cat ->
                    E.text cat
                        |> E.el
                            ([ E.paddingEach
                                { bottom = 4
                                , top = 0
                                , left = 0
                                , right = 0
                                }
                             , E.width E.fill
                             , fonts
                             , EE.onClick (SelectCategory cat)
                             , E.pointer
                             ]
                                ++ navStyle cat
                            )
                )
                categories
    in
    E.row
        [ ER.navigation, E.spacing 8, E.width E.fill, E.padding 8 ]
        options


viewSearchFilter : String -> E.Element Msg
viewSearchFilter searchString =
    E.el [ E.padding 5, E.width E.fill ] <|
        EI.search
            [ E.padding 5
            , fonts
            , EF.size 13
            , EB.width 1
            , EB.rounded 6
            , EB.color (E.rgba 0.0 0.0 0.0 0.15)
            , EBa.color (E.rgb (249.0 / 256.0) (249.0 / 256.0) (249.0 / 256.0))
            ]
            { onChange = SearchInput
            , text = searchString
            , label = EI.labelHidden "Search"
            , placeholder =
                Just
                    (EI.placeholder [] (E.text "Search"))
            }


toElementColor color =
    let
        rgba =
            Color.toRgba color
    in
    E.rgba (rgba.red / 256.0) (rgba.green / 256.0) (rgba.blue / 256.0) 0.2


emojiBackground index =
    case modBy 5 index of
        0 ->
            toElementColor CQ.accent5_0

        1 ->
            toElementColor CQ.accent5_1

        2 ->
            toElementColor CQ.accent5_2

        3 ->
            toElementColor CQ.accent5_3

        4 ->
            toElementColor CQ.accent5_4

        _ ->
            emojiBackground 0


viewEmoji : Int -> String -> Emoji -> E.Element Msg
viewEmoji index url emoji =
    E.el
        [ EE.onClick (SelectEmoji emoji.name)
        , E.height (E.px 64)
        , E.width (E.px 64)
        , E.centerX
        , EB.rounded 10
        , EB.width 2
        , EB.color
            (E.rgba 0.0
                0.0
                0.0
                (if emoji.isSelected then
                    0.5

                 else
                    0.0
                )
            )
        , EBa.color (emojiBackground index)
        ]
    <|
        E.el
            ([ E.centerX
             , E.centerY
             , E.height (E.px 32)
             , E.width (E.px 32)
             , E.pointer
             ]
                ++ (spriteCssAttribute (32 * emoji.x) (32 * emoji.y) url
                        |> List.map E.htmlAttribute
                   )
            )
            E.none


viewEmojiDetail : String -> Maybe Emoji -> E.Element Msg
viewEmojiDetail url emoji =
    E.row
        [ E.padding 8, E.centerX, E.centerY, E.spacing 8 ]
        (Maybe.map
            (\em ->
                [ E.el
                    ([ E.height
                        (E.px 32)
                     , E.width (E.px 32)
                     ]
                        ++ (spriteCssAttribute (32 * em.x) (32 * em.y) url
                                |> List.map E.htmlAttribute
                           )
                    )
                    E.none
                , E.el
                    [ fonts ]
                    (E.text em.name)
                ]
            )
            emoji
            |> Maybe.withDefault
                [ E.el
                    [ fonts ]
                    (E.text
                        "No emoji selected"
                    )
                ]
        )


emojisByCategory : String -> List Emoji -> List Emoji
emojisByCategory category =
    List.filter (.category >> (==) category)


emojisFilteredByString : String -> List Emoji -> List Emoji
emojisFilteredByString string =
    List.filter (.name >> String.contains string)


groupsOf : Int -> List a -> List (List a)
groupsOf size xs =
    groupsOfWithStep size size xs


groupsOfWithStep : Int -> Int -> List a -> List (List a)
groupsOfWithStep size step xs =
    let
        thisGroup =
            List.take size xs

        xs_ =
            List.drop step xs

        okayArgs =
            size > 0 && step > 0

        okayLength =
            size == List.length thisGroup
    in
    if okayArgs && okayLength then
        thisGroup :: groupsOfWithStep size step xs_

    else
        []


viewEmojis : String -> List Emoji -> E.Element Msg
viewEmojis url emojis =
    let
        length =
            List.length emojis

        x =
            9

        y =
            length // x

        columns =
            List.range 0 (x - 1) |> List.map (\a -> E.px 64)

        rows =
            groupsOf y
                (List.indexedMap (\i emoji -> viewEmoji i url emoji) emojis)
    in
    E.row
        [ EBa.color (E.rgb 1.0 1.0 1.0)
        , E.scrollbarY
        , E.spacing 3
        , E.htmlAttribute (Html.Attributes.id "emojis")
        , E.height (E.px 300)
        ]
        (List.map (E.column [ E.spacing 3 ]) rows)


fonts =
    EF.family
        [ EF.typeface "Helvetica"
        , EF.typeface "Trebuchet MS"
        , EF.typeface "Verdana"
        , EF.typeface "sans-serif"
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
    E.column
        [ E.centerX
        , E.centerY
        , EB.width 1
        , EB.rounded 6
        , EB.solid
        , EB.color (E.rgba 0.0 0.0 0.0 0.15)
        , EBa.color (E.rgb (249.0 / 256.0) (249.0 / 256.0) (249.0 / 256.0))
        ]
        [ viewCategorySelector model.selectedCategory model.categories
        , viewSearchFilter (Maybe.withDefault "" model.searchString)
        , Maybe.map (viewEmojis model.emojisUrl) selectedEmojis
            |> Maybe.withDefault
                (E.text "No category selected"
                    |> E.el
                        [ fonts ]
                )
        , viewEmojiDetail model.emojisUrl (List.filter .isSelected model.emojis |> List.head)
        ]
        |> E.layout []


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown
        (JD.field "key" JD.string)
        |> Sub.map (Debug.log "subscriptions")
        |> Sub.map
            (\key ->
                case key of
                    "ArrowLeft" ->
                        CursorMove Left

                    "ArrowRight" ->
                        CursorMove Right

                    "ArrowDown" ->
                        CursorMove Down

                    "ArrowUp" ->
                        CursorMove Up

                    _ ->
                        Noop
            )


main : Program JD.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
