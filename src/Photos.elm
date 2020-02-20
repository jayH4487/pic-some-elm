module Photos exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Http
import Json.Decode exposing (Decoder, Value, at, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


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


type Status
    = Loading
    | Loaded (List Photo)
    | Errored String


type alias Model =
    { status : Status
    }


initialModel : Model
initialModel =
    { status = Loading
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    ( { model | status = Loaded photos }, Cmd.none )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] <|
        case model.status of
            Loaded photos ->
                [ viewPhotos photos ]

            Loading ->
                [ text "Loading..." ]

            Errored error ->
                [ text "Errored" ]


viewPhotos : List Photo -> Html Msg
viewPhotos photos =
    main_ [ class "photos" ] (List.indexedMap viewImage photos)


viewImage : Int -> Photo -> Html Msg
viewImage index photo =
    div [ class "image-container", class (getClass index) ]
        [ img [ src photo.url, class "image-grid" ] []
        ]


getClass : Int -> String
getClass index =
    if modBy 5 index == 0 then
        "big"

    else if modBy 6 index == 0 then
        "wide"

    else
        ""
