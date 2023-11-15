port module Ports exposing (audioPortFromJS, audioPortToJS)

import Json.Decode
import Json.Encode

port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg
