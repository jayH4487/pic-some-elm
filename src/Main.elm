module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, href, src, width)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Http
import Photo exposing (Photo, photosDecoder)
import Process
import Task
import Time
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, s)



--MAIN


main : Program () Model Msg
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


type alias CartItems =
    List Photo


type Status
    = Loading
    | Loaded (List Photo) String
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
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "https://raw.githubusercontent.com/bobziroll/scrimba-react-bootcamp-images/master/images.json"
        , expect = Http.expectJson GotPhotos photosDecoder
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    ( { key = key
      , page = urlToPage url
      , status = Loading
      , cartItems = []
      , orderBtnText = "Place Order"
      }
    , initialCmd
    )


urlToPage : Url -> Page
urlToPage url =
    Parser.parse parser url
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
            ( { model | page = urlToPage url }, Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    ( { model | status = Loaded photos "" }, Cmd.none )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        MouseEntered hoveredPhotoId ->
            ( { model | status = setHoveredPhotoId hoveredPhotoId model.status }, Cmd.none )

        MouseLeft ->
            ( { model | status = setHoveredPhotoId "" model.status }, Cmd.none )

        ToggleFavorite photoId ->
            ( { model | status = setFavorite photoId model.status }, Cmd.none )

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


setHoveredPhotoId : String -> Status -> Status
setHoveredPhotoId hoveredPhotoId status =
    case status of
        Loaded photos _ ->
            Loaded photos hoveredPhotoId

        Loading ->
            status

        Errored _ ->
            status


setFavorite : String -> Status -> Status
setFavorite photoId status =
    case status of
        Loaded photos hoveredPhotoId ->
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
            Loaded updatedPhotos hoveredPhotoId

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
        [ div [] [ viewHeader model.cartItems ]
        , content model
        ]
    }


viewHeader : CartItems -> Html Msg
viewHeader cartItems =
    let
        shoppingCartIcon =
            if List.length cartItems > 0 then
                "ri-shopping-cart-fill ri-fw ri-2x"

            else
                "ri-shopping-cart-line ri-fw ri-2x"
    in
    header []
        [ a [ href "/" ] [ text "Pic Some" ]
        , a [ href "/cart" ] [ i [ class shoppingCartIcon ] [] ]
        ]


content : Model -> Html Msg
content model =
    case model.page of
        Home ->
            viewHome model

        Cart ->
            viewCart model

        NotFound ->
            viewNotFound


viewHome : Model -> Html Msg
viewHome model =
    div [] <|
        case model.status of
            Loaded photos hoveredPhotoId ->
                [ viewPhotos photos hoveredPhotoId model.cartItems ]

            Loading ->
                [ text "Loading..." ]

            Errored error ->
                [ text "Errored" ]


viewPhotos : List Photo -> String -> CartItems -> Html Msg
viewPhotos photos hoveredPhotoId cartItems =
    main_ [ class "photos" ] (List.indexedMap (viewImage hoveredPhotoId cartItems) photos)


viewImage : String -> CartItems -> Int -> Photo -> Html Msg
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


viewHeartIcon : String -> Photo -> Html Msg
viewHeartIcon hoveredPhotoId photo =
    if photo.isFavorite then
        i [ class "ri-heart-fill favorite", onClick (ToggleFavorite photo.id) ] []

    else if hoveredPhotoId == photo.id then
        i [ class "ri-heart-line favorite", onClick (ToggleFavorite photo.id) ] []

    else
        i [] []


viewCartIcon : String -> Photo -> CartItems -> Html Msg
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


viewCart : Model -> Html Msg
viewCart model =
    let
        totalCost =
            5.99 * toFloat (List.length model.cartItems)
    in
    main_ [ class "cart-page" ] <|
        h1 [] [ text "Check out" ]
            :: List.map viewCartItems model.cartItems
            ++ [ p [ class "total-cost" ]
                    [ text "Total: "
                    , costItem { cultureCode = "en-GB", currency = "GBP", cost = totalCost }
                    ]
               , div
                    [ class "order-button" ]
                    [ button [ onClick OrderPlaced ] [ text model.orderBtnText ] ]
               ]


costItem : CostItem -> Html Msg
costItem { cultureCode, currency, cost } =
    Html.node "cost-item"
        [ Html.Attributes.attribute "culture-code" cultureCode
        , Html.Attributes.attribute "currency" currency
        , Html.Attributes.attribute "cost" (String.fromFloat cost)
        ]
        []


viewCartItems : Photo -> Html Msg
viewCartItems photo =
    div [ class "cart-item" ]
        [ i [ class "ri-delete-bin-line", onClick (RemovedFromCart photo.id) ] []
        , img [ src photo.url, width 130 ] []
        , p [] [ text "£5.99" ]
        ]


viewNotFound : Html Msg
viewNotFound =
    p [] [ text "Page Not Found" ]
