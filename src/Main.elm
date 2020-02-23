module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Http
import Photo exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)



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


type alias Model =
    { key : Nav.Key
    , page : Page
    , status : Status
    , cartItems : CartItems
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
    | AddToCart Photo
    | RemoveFromCart String


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

        AddToCart photo ->
            ( { model | cartItems = model.cartItems ++ [ photo ] }, Cmd.none )

        RemoveFromCart photoId ->
            let
                updatedCartItems =
                    List.filter (\photo -> photoId /= photo.id) model.cartItems
            in
            ( { model | cartItems = updatedCartItems }, Cmd.none )


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
        [ div [] [ viewHeader ]
        , content model
        ]
    }


viewHeader : Html Msg
viewHeader =
    header []
        [ a [ href "/" ] [ text "Pic Some" ]
        , a [ href "/cart" ] [ i [ class "ri-shopping-cart-line ri-fw ri-2x" ] [] ]
        ]


content : Model -> Html Msg
content model =
    case model.page of
        Home ->
            viewHome model

        Cart ->
            viewCart

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
        i [ class "ri-shopping-cart-fill cart", onClick (RemoveFromCart photo.id) ] []

    else if hoveredPhotoId == photo.id then
        i [ class "ri-add-circle-line cart", onClick (AddToCart photo) ] []

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


viewCart : Html Msg
viewCart =
    main_ [ class "cart-page" ] [ h1 [] [ text "Check out" ] ]


viewNotFound : Html Msg
viewNotFound =
    p [] [ text "Page Not Found" ]
