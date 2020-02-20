module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
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


type alias Model =
    { key : Nav.Key
    , page : Page

    -- , photos : List String
    }


type Page
    = Home
    | Cart
    | NotFound


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key, page = urlToPage url }, Cmd.none )


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
            ( { model | page = urlToPage url }
            , Cmd.none
            )


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


viewHeader : Html msg
viewHeader =
    header []
        [ a [ href "/" ] [ text "Pic Some" ]
        , a [ href "/cart" ] [ i [ class "ri-shopping-cart-line ri-fw ri-2x" ] [] ]
        ]


content : Model -> Html msg
content model =
    case model.page of
        Home ->
            viewHome

        Cart ->
            viewCart

        NotFound ->
            viewNotFound


viewHome : Html msg
viewHome =
    main_ [ class "photos" ] [ h1 [] [ text "Images go here" ] ]


viewCart : Html msg
viewCart =
    main_ [ class "cart-page" ] [ h1 [] [ text "Check out" ] ]


viewNotFound : Html msg
viewNotFound =
    p [] [ text "Page Not Found" ]
