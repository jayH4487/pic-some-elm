module Photo exposing (Photo, photosDecoder)

import Json.Decode exposing (Decoder, bool, list, string, succeed)
import Json.Decode.Pipeline exposing (required)


type alias Photo =
    { url : String
    , id : String
    , isFavorite : Bool
    }


photosDecoder : Decoder (List Photo)
photosDecoder =
    list photoDecoder


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> Json.Decode.Pipeline.required "url" string
        |> Json.Decode.Pipeline.required "id" string
        |> Json.Decode.Pipeline.required "isFavorite" bool
