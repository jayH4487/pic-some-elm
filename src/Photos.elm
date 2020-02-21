module Photos exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Http
import Json.Decode exposing (Decoder, bool, list, string, succeed)
import Json.Decode.Pipeline exposing (required)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Photo =
    { url : String
    , id : String
    , isFavorite : Bool
    }


type alias CartItems =
    List Photo


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , cartItems : CartItems
    }


initialModel : Model
initialModel =
    { status = Loading
    , cartItems = []
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "https://raw.githubusercontent.com/bobziroll/scrimba-react-bootcamp-images/master/images.json"
        , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder)
        }


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> Json.Decode.Pipeline.required "url" string
        |> Json.Decode.Pipeline.required "id" string
        |> Json.Decode.Pipeline.required "isFavorite" bool


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, initialCmd )


type Msg
    = GotPhotos (Result Http.Error (List Photo))
    | MouseEntered String
    | MouseLeft
    | ToggleFavorite String
    | AddToCart Photo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


view : Model -> Html Msg
view model =
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
        i [ class "ri-shopping-cart-fill cart" ] []

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
