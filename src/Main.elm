module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, src, width)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Http
import Photo exposing (Photo, photosDecoder)
import Process
import Task
import Time
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, s)



--MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



--MODEL


type alias Flags =
    { basePath : String }


type alias CartItems =
    List Photo


type alias HoveredPhotoId =
    String


type Status
    = Loading
    | Loaded (List Photo)
    | Errored String


type Page
    = Home
    | Cart
    | NotFound


type alias CostItem =
    { cultureCode : String
    , currency : String
    , cost : Float
    }


type alias Model =
    { key : Nav.Key
    , page : Page
    , status : Status
    , cartItems : CartItems
    , orderBtnText : String
    , hoveredPhotoId : HoveredPhotoId
    , flags : Flags
    , url : Url.Url
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "https://raw.githubusercontent.com/bobziroll/scrimba-react-bootcamp-images/master/images.json"
        , expect = Http.expectJson GotPhotos photosDecoder
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init ({ basePath } as flags) url key =
    ( { key = key
      , page = urlToPage basePath url
      , status = Loading
      , cartItems = []
      , orderBtnText = "Place Order"
      , hoveredPhotoId = ""
      , flags = flags
      , url = url
      }
    , initialCmd
    )


urlToPage : String -> Url -> Page
urlToPage basePath url =
    { url | path = String.replace basePath "" url.path }
        |> Parser.parse parser
        |> Maybe.withDefault NotFound


parser : Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Cart (Parser.s "cart")
        ]



--UPDATE


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url
    | GotPhotos (Result Http.Error (List Photo))
    | MouseEntered String
    | MouseLeft
    | ToggleFavorite String
    | AddedToCart Photo
    | RemovedFromCart String
    | OrderPlaced
    | OrderProcessed Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ChangedUrl url ->
            ( { model | page = urlToPage model.flags.basePath url, url = url }, Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    ( { model | status = Loaded photos }, Cmd.none )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        MouseEntered hoveredPhotoId ->
            ( { model | hoveredPhotoId = hoveredPhotoId }, Cmd.none )

        MouseLeft ->
            ( { model | hoveredPhotoId = "" }, Cmd.none )

        ToggleFavorite photoId ->
            ( { model | status = toggleFavorite photoId model.status }, Cmd.none )

        AddedToCart photo ->
            ( { model | cartItems = model.cartItems ++ [ photo ] }, Cmd.none )

        RemovedFromCart photoId ->
            let
                updatedCartItems =
                    List.filter (\photo -> photoId /= photo.id) model.cartItems
            in
            ( { model | cartItems = updatedCartItems }, Cmd.none )

        OrderPlaced ->
            ( { model | orderBtnText = "Ordering..." }
            , Task.perform OrderProcessed (Process.sleep 3000 |> Task.andThen (\_ -> Time.now))
            )

        OrderProcessed _ ->
            ( { model | orderBtnText = "Place Order", cartItems = [] }, Cmd.none )


toggleFavorite : String -> Status -> Status
toggleFavorite photoId status =
    case status of
        Loaded photos ->
            let
                updatedPhotos =
                    List.map
                        (\photo ->
                            if photo.id == photoId then
                                { photo | isFavorite = not photo.isFavorite }

                            else
                                photo
                        )
                        photos
            in
            Loaded updatedPhotos

        Loading ->
            status

        Errored _ ->
            status


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "App"
    , body =
        [ div [] [ viewHeader model ]
        , content model
        ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    let
        shoppingCartIcon =
            if List.length model.cartItems > 0 then
                "ri-shopping-cart-fill ri-fw ri-2x"

            else
                "ri-shopping-cart-line ri-fw ri-2x"

        basePath =
            if model.flags.basePath == "/index.html" then
                ""

            else
                model.flags.basePath
    in
    div []
        [ p []
            [-- text (Url.toString model.url ++ "-" ++ model.flags.basePath)
            ]
        , header
            []
            [ a [ href (basePath ++ "/") ] [ text "Pic Some" ]
            , a [ href (basePath ++ "/cart") ] [ i [ class shoppingCartIcon ] [] ]
            ]
        ]


content : Model -> Html Msg
content model =
    case model.page of
        Home ->
            viewHomePage model

        Cart ->
            viewCartPage model

        NotFound ->
            viewHomePage model


viewHomePage : Model -> Html Msg
viewHomePage model =
    div [] <|
        case model.status of
            Loaded photos ->
                [ viewPhotos photos model.hoveredPhotoId model.cartItems ]

            Loading ->
                [ text "Loading..." ]

            Errored error ->
                [ text "Errored" ]


viewPhotos : List Photo -> HoveredPhotoId -> CartItems -> Html Msg
viewPhotos photos hoveredPhotoId cartItems =
    main_ [ class "photos" ] (List.indexedMap (viewImage hoveredPhotoId cartItems) photos)


viewImage : HoveredPhotoId -> CartItems -> Int -> Photo -> Html Msg
viewImage hoveredPhotoId cartItems index photo =
    div
        [ class "image-container"
        , class (getClass index)
        , onMouseEnter (MouseEntered photo.id)
        , onMouseLeave MouseLeft
        ]
        [ img [ src photo.url, class "image-grid" ] []
        , viewHeartIcon hoveredPhotoId photo
        , viewCartIcon hoveredPhotoId photo cartItems
        ]


viewHeartIcon : HoveredPhotoId -> Photo -> Html Msg
viewHeartIcon hoveredPhotoId photo =
    if photo.isFavorite then
        i [ class "ri-heart-fill favorite", onClick (ToggleFavorite photo.id) ] []

    else if hoveredPhotoId == photo.id then
        i [ class "ri-heart-line favorite", onClick (ToggleFavorite photo.id) ] []

    else
        i [] []


viewCartIcon : HoveredPhotoId -> Photo -> CartItems -> Html Msg
viewCartIcon hoveredPhotoId photo cartItems =
    if List.member photo.id (List.map .id cartItems) then
        i [ class "ri-shopping-cart-fill cart", onClick (RemovedFromCart photo.id) ] []

    else if hoveredPhotoId == photo.id then
        i [ class "ri-add-circle-line cart", onClick (AddedToCart photo) ] []

    else
        i [] []


getClass : Int -> String
getClass index =
    if modBy 5 index == 0 then
        "big"

    else if modBy 6 index == 0 then
        "wide"

    else
        ""


viewCartPage : Model -> Html Msg
viewCartPage model =
    let
        totalCost =
            5.99 * toFloat (List.length model.cartItems)
    in
    main_ [ class "cart-page" ] <|
        h1 [] [ text "Check out" ]
            :: List.map (viewCartItems model.hoveredPhotoId) model.cartItems
            ++ [ p [ class "total-cost" ]
                    [ text "Total: "
                    , costItem { cultureCode = "en-GB", currency = "GBP", cost = totalCost }
                    ]
               , viewPlaceOrderBtn model
               ]


viewCartItems : HoveredPhotoId -> Photo -> Html Msg
viewCartItems hoveredPhotoId photo =
    div [ class "cart-item" ]
        [ i
            [ classList
                [ ( "ri-delete-bin-fill", hoveredPhotoId == photo.id )
                , ( "ri-delete-bin-line", hoveredPhotoId /= photo.id )
                ]
            , onClick (RemovedFromCart photo.id)
            , onMouseEnter (MouseEntered photo.id)
            , onMouseLeave MouseLeft
            ]
            []
        , img [ src photo.url, width 130 ] []
        , p [] [ text "£5.99" ]
        ]


costItem : CostItem -> Html Msg
costItem { cultureCode, currency, cost } =
    Html.node "cost-item"
        [ Html.Attributes.attribute "culture-code" cultureCode
        , Html.Attributes.attribute "currency" currency
        , Html.Attributes.attribute "cost" (String.fromFloat cost)
        ]
        []


viewPlaceOrderBtn : Model -> Html Msg
viewPlaceOrderBtn model =
    if List.length model.cartItems > 0 then
        div
            [ class "order-button" ]
            [ button [ onClick OrderPlaced ] [ text model.orderBtnText ] ]

    else
        p [] [ text "Cart is empty." ]


viewNotFound : Html Msg
viewNotFound =
    p [] [ text "Page Not Found" ]
